namespace ViewModel

open System
open ViewModel.Gaussians
open ViewModel.MonteCarlo
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
        (*
        
        Lecture 5
        
        *)
    let EUcallPrice (S0:double) (K:double) (r:double) (sg:double) (T:double) =
        let d1 = ((log(S0/K)) + (r + sg*sg/2.0)*T) / (sg * (sqrt T))
        S0 * Phi d1 - K * (Phi (d1 - sg * sqrt T))*exp (-r*T)

    let EUputPrice (S0:double) (K:double) (r:double) (sg:double) (T:double) =
        let d1 = ((log(S0/K)) + (r + sg*sg/2.0)*T) / (sg * (sqrt T))
        K*exp(-r*T) * Phi (sg * (sqrt T) - d1) - S0 * Phi(-d1)

    let EUdeltaCall (S0:double) (K:double) (r:double) (sg:double) (T:double) = 
        let d1 = (log(S0/K) + (r + sg*sg/2.0)*T) / (sg * T)
        Phi (d1)

    let EUdeltaPut (S0:double) (K:double) (r:double) (sg:double) (T:double) = 
        let d1 = (log(S0/K) + (r + sg*sg/2.0)*T) / (sg * T)
        -Phi (-d1)

    (*
    
    UEWr
    *)


    let futuresLong S0 K r T =
        S0 - K*exp(-r*T)

    let futuresShort S0 K r T = 
        -futuresLong S0 K r T
        (*
        http://uu.diva-portal.org/smash/get/diva2:301070/FULLTEXT01.pdf
        *)

    let AsianGeoCallPrice (S0:double) (K:double) (r:double) (sg:double) (T:double) =
        let sgg = sg/sqrt(3.0)
        let b = (r-(sgg*sgg/2.0))/2.0
        let d1 = ((log(S0/K)) + (b + sgg*sgg/2.0)*T) / (sgg * (sqrt T))
        let d2 = d1 - (sgg *sqrt(T))
        S0*exp((b-r)*T)*Phi(d1) - K*exp(-r*T)*Phi(d2)

    let AsianGeoPutPrice (S0:double) (K:double) (r:double) (sg:double) (T:double) = 
        let sgg = sg/sqrt(3.0)
        let b = (r-(sgg*sgg/2.0))/2.0
        let d1 = ((log(S0/K)) + (b + sgg*sgg/2.0)*T) / (sgg * (sqrt T))
        let d2 = d1 - (sgg *sqrt(T))
        K*exp(-r*T)*Phi(-d2) - S0*exp((b-r)*T)*Phi(-d1)

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
            let MCruns = if inputs.CalculationsParameters.ContainsKey "monteCarlo::runs" then int inputs.CalculationsParameters.[ "monteCarlo::runs" ] else 1 // lookup FX rate
            let MCsteps = if inputs.CalculationsParameters.ContainsKey "monteCarlo::steps" then int inputs.CalculationsParameters.[ "monteCarlo::steps" ] else 10000 // lookup FX rate
            let seed = if inputs.CalculationsParameters.ContainsKey "random::seed" then int inputs.CalculationsParameters.[ "random::seed" ] else 12345 // lookup FX rate
            let preffered = 
                if inputs.CalculationsParameters.ContainsKey "monteCarlo::prefer" then 
                    if inputs.CalculationsParameters.[ "monteCarlo::prefer" ]="YES" then true else false
                else false // lookup FX rate
            let V = 
                match inputs.Option.OptionType with
                    | "European Call" -> if preffered then MonteCarlo.europeanCall2 MCruns MCsteps S0 finalK rRate sg years seed else EUcallPrice S0 finalK rRate sg years
                    | "European Put" -> if preffered then MonteCarlo.europeanPut2 MCruns MCsteps S0 finalK rRate sg years seed else EUputPrice S0 finalK rRate sg years
                    | "American Call" -> MonteCarlo.americanCall2 MCruns MCsteps S0 finalK rRate sg years seed
                    | "American Put" -> MonteCarlo.americanPut2 MCruns MCsteps S0 finalK rRate sg years seed
                    | "Asian Put" -> MonteCarlo.asianPut MCruns MCsteps S0 finalK rRate sg years seed
                    | "Asian Call" -> MonteCarlo.asianCall MCruns MCsteps S0 finalK rRate sg years seed
                    | "Asian Geo Put" -> AsianGeoPutPrice S0 finalK rRate sg years
                    | "Asian Geo Call" -> AsianGeoCallPrice S0 finalK rRate sg years
                    | "Futures Long" -> futuresLong S0 finalK rRate years
                    | "Futures Short" -> futuresShort S0 finalK rRate years
                    | x -> 0.0
            let delta = 
                match inputs.Option.OptionType with
                    | "European Call" -> EUdeltaCall S0 finalK rRate sg years
                    | "European Put" -> EUdeltaPut S0 finalK rRate sg years
                    | x -> Double.NaN
            let money:Money = { Value = V*double amount; Currency = finalCcy;}
            {M=money;Delta = delta}
        else
            let money:Money = {Value = 0.0; Currency = finalCcy;}
            {M=money;Delta = 0.0}
