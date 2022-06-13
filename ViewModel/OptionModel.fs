namespace ViewModel

open System

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
        Expiry    : DateTime
        Currency  : string
        Principal : int64
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
            OptionName = sprintf "Payment%04d" (OptionRecord.sysRandom.Next(9999))
            OptionType = "First"
            Expiry    = (DateTime.Now.AddMonths (OptionRecord.sysRandom.Next(1, 6))).Date
            Currency  = knownCurrencies.[ OptionRecord.sysRandom.Next(knownCurrencies.Length) ]
            Principal = int64 (OptionRecord.sysRandom.Next())
        }

(* Complete set of data required for valuation *)
type OptionValuationInputs = 
    {
        Trade : OptionRecord
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
    member this.Calculate() : OptionPrice = 
        let tradeCcy = inputs.Trade.Currency

        let targetCcy = match inputs.CalculationsParameters.TryFind "valuation::baseCurrency" with
                         | Some ccy -> ccy
                         | None -> tradeCcy

        let fxRateKey = sprintf "FX::%s%s" targetCcy tradeCcy

        let fxRate = if inputs.Data.ContainsKey fxRateKey then float inputs.Data.[ fxRateKey ] else 1.0 // lookup FX rate
        let finalValue = (float inputs.Trade.Principal) / fxRate
        let finalCcy = if inputs.Data.ContainsKey fxRateKey then targetCcy else tradeCcy
        let timeDiff = inputs.Trade.Expiry - DateTime.Now
        let years = timeDiff.TotalDays/365.25
        let rRate = if inputs.Data.ContainsKey "CFG::R" then float inputs.Data.[ "CFG::R" ] else 0.0
        let V = finalValue/exp(years*rRate)

        let money:Money = { Value = V; Currency = finalCcy;}
        {M=money;Delta = 0.0}
