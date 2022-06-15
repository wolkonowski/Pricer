namespace ViewModel

open System
open ViewModel.Gaussians
(* A type representing given amount of money in specific currency. Very bare bones, could be extended in various ways. Some examples:
1. Multiplication by float so that $1 * 100 = $100.
2. Addition to other Money instance so that $1 + $2 = $3, but 1 zl + $1 = <exception thrown> *)

type OptionPrice = 
    {
        M : Money
        Delta : float
    }

(* Model for Payment trade. *)
type OptionRecord =
    {
        OptionName : string
        OptionType : string
        Stock : string
        Expiry    : DateTime
        Currency  : string
        Principal : int64
        Amount : int64
    }
    
    (* Simple utility method for creating a random payment. *)
    static member sysRandom = System.Random()
    static member Random(configuration : CalculationConfiguration) = 
        (* We pick a random currency either from given short list, or from valuation::knownCurrencies config key *)
        let knownCurrenciesDefault = [| "EUR"; "USD"; "PLN"; |]
        
        let knownCurrencies = if configuration.ContainsKey "valuation::knownCurrencies" 
                              then configuration.["valuation::knownCurrencies"].Split([|' '|])
                              else knownCurrenciesDefault
        
        {
            OptionName = sprintf "Option%04d" (OptionRecord.sysRandom.Next(9999))
            OptionType = "European Put"
            Stock = "NASDAQ::TSLA"
            Expiry    = (DateTime.Now.AddMonths (OptionRecord.sysRandom.Next(1, 6))).Date
            Currency  = knownCurrencies.[ OptionRecord.sysRandom.Next(knownCurrencies.Length) ]
            Principal = int64 (OptionRecord.sysRandom.Next(100))
            Amount = int64 (OptionRecord.sysRandom.Next(100))
        }

(* Complete set of data required for valuation *)
type OptionValuationInputs = 
    {
        Option : OptionRecord
        Data : DataConfiguration
        CalculationsParameters: CalculationConfiguration
    }

(* The valuation model for Payment. We may have multiple valuation models implementations per given trade type, or have a valuation model that handles multiple trade types. *)
type OptionValuationModel(inputs: OptionValuationInputs) = 
    (* Calculate() method returns a value of given trade. This one is very simple, yet demonstrates some concepts.
    
    It will try to return the result in the global default currency as configured by valuation::baseCurrency key.

    If the valuation::baseCurrency is not defined or we are unable to obtain the FX rate FX::<targetCcy><tradeCcy>, 
    we simply return the value using the trade currency.

    *)
    let Phi x = 
        Gaussians.normcdf x 

    let callPrice (S0:double) (K:double) (r:double) (sg:double) (T:double) =
        let d1 = ((log(S0/K)) + (r + sg*sg/2.0)*T) / (sg * (sqrt T))
        S0 * Phi d1 - K * (Phi (d1 - sg * sqrt T))*exp (-r*T)

    let putPrice (S0:double) (K:double) (r:double) (sg:double) (T:double) =
        let d1 = ((log(S0/K)) + (r + sg*sg/2.0)*T) / (sg * (sqrt T))
        K*exp(-r*T) * Phi (sg * (sqrt T) - d1) - S0 * Phi(-d1)

    let deltaCall (S0:double) (K:double) (r:double) (sg:double) (T:double) = 
        let d1 = (log(S0/K) + (r + sg*sg/2.0)*T) / (sg * T)
        Phi (d1)

    let deltaPut (S0:double) (K:double) (r:double) (sg:double) (T:double) = 
        let d1 = (log(S0/K) + (r + sg*sg/2.0)*T) / (sg * T)
        -Phi (-d1)


    member this.Calculate() : OptionPrice = 
        let currency = inputs.Option.Currency

        let targetCcy = match inputs.CalculationsParameters.TryFind "valuation::baseCurrency" with
                         | Some ccy -> ccy
                         | None -> currency

        let fxRateKey = sprintf "FX::%s%s" targetCcy currency

        let fxRate = if inputs.Data.ContainsKey fxRateKey then float inputs.Data.[ fxRateKey ] else 1.0 // lookup FX rate
        
        let finalK = (float inputs.Option.Principal) / fxRate
        let finalCcy = if inputs.Data.ContainsKey fxRateKey then targetCcy else currency
        let timeDiff = inputs.Option.Expiry - DateTime.Now
        let accept = match inputs.CalculationsParameters.TryFind "valuation::baseCurrency" with
            | Some x -> x
            | None -> ""
        if timeDiff>TimeSpan(0L) && accept=finalCcy then
            let years = timeDiff.TotalDays/365.25

            let defSg = if inputs.Data.ContainsKey "CFG::SIGMA" then float inputs.Data.[ "CFG::SIGMA" ] else 0.0
            let rRate = if inputs.Data.ContainsKey "CFG::R" then float inputs.Data.[ "CFG::R" ] else 0.0
            let S0 = if inputs.Data.ContainsKey inputs.Option.Stock then float inputs.Data.[inputs.Option.Stock] else 0.0
            let optionSg = inputs.Option.Stock+"::SG"
            let amount = inputs.Option.Amount
            let sg = if inputs.Data.ContainsKey optionSg then float inputs.Data.[ optionSg ] else defSg
            let V = 
                match inputs.Option.OptionType with
                    | "European Call" -> callPrice S0 finalK rRate sg years
                    | "European Put" -> putPrice S0 finalK rRate sg years
                    | x -> 0.0
            let delta = 
                match inputs.Option.OptionType with
                    | "European Call" -> deltaCall S0 finalK rRate sg years
                    | "European Put" -> deltaPut S0 finalK rRate sg years
                    | x -> 0.0
            let money:Money = { Value = V*double amount; Currency = finalCcy;}
            {M=money;Delta = delta}
        else
            let money:Money = {Value = 0.0; Currency = finalCcy;}
            {M=money;Delta = 0.0}
