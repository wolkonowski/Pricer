namespace ViewModel

open System
open System.Collections.ObjectModel
open LiveCharts;
open LiveCharts.Wpf;

//Strating point of the viewmodel that drives the UI
//It aggregates all relevant parts of the UI, and exposes them via properties
type MainViewModel() = 
    inherit ViewModelBase()

    let trades = ObservableCollection<PaymentViewModel>()
    let options = ObservableCollection<OptionViewModel>()
    let data = ObservableCollection<ConfigurationViewModel>()
    let calculationParameters = ObservableCollection<ConfigurationViewModel>()

    let getDataConfiguration () = data |> Seq.map (fun conf -> (conf.Key , conf.Value)) |> Map.ofSeq
    let getCalculationConfiguration () = calculationParameters |> Seq.map (fun conf -> (conf.Key , conf.Value)) |> Map.ofSeq
    
    (* add some dummy data rows *)
    do
        data.Add(ConfigurationViewModel { Key = "FX::USDPLN"; Value = "3.76" })
        data.Add(ConfigurationViewModel { Key = "FX::USDEUR"; Value = "0.87" })
        data.Add(ConfigurationViewModel { Key = "FX::EURGBP"; Value = "0.90" })
        data.Add(ConfigurationViewModel { Key = "CFG::R"; Value = "0.01"})
        data.Add(ConfigurationViewModel { Key = "CFG::SIGMA"; Value = "0.10"})
        data.Add(ConfigurationViewModel {Key = "NASDAQ::TSLA";Value = "651.30"})
        data.Add(ConfigurationViewModel {Key = "NASDAQ::TSLA::SG";Value = "0.1"})

        calculationParameters.Add(ConfigurationViewModel { Key = "monteCarlo::runs"; Value = "100" })
        calculationParameters.Add(ConfigurationViewModel { Key = "monteCarlo::steps"; Value = "10000" })
        calculationParameters.Add(ConfigurationViewModel { Key = "monteCarlo::prefer"; Value = "NO" })
        calculationParameters.Add(ConfigurationViewModel { Key = "random::seed"; Value = "12345" })
        calculationParameters.Add(ConfigurationViewModel { Key = "valuation::baseCurrency"; Value = "USD" })
        calculationParameters.Add(ConfigurationViewModel { Key = "valuation::knownCurrencies"; Value = "USD PLN EUR GBP" })
        calculationParameters.Add(ConfigurationViewModel { Key = "valuation::knownMarkets"; Value = "NASDAQ"})
        calculationParameters.Add(ConfigurationViewModel { Key = "methodology::bumpRisk"; Value = "True" })
        calculationParameters.Add(ConfigurationViewModel { Key = "methodology::bumpSize"; Value = "0.0001" })

    let summary = ObservableCollection<SummaryRow>()
    let refreshSummary() = 
        summary.Clear()
        
        let t_val = trades |> Seq.choose(fun t -> t.Value) // find correctly evaluated trades
        let o_val = options |> Seq.choose(fun o -> o.Value)
        let merged = Seq.append t_val o_val
        merged
        |> Seq.groupBy(fun m -> m.Currency)  // group by currency
        |> Seq.map(fun (ccy, v) -> { Currency = ccy; Value = v |> Seq.map (fun m -> m.Value) |> Seq.sum }) // extract values, calculate a sum
        |> Seq.iter(summary.Add) // add to summary page

        
    (* trade commands *)
    let calculateFun _ = do
            trades |> Seq.iter(fun trade -> trade.Calculate(getDataConfiguration (), getCalculationConfiguration ()))
            refreshSummary()

    let calculate = SimpleCommand calculateFun

    let addTrade = SimpleCommand(fun _ -> 
            let currentConfig = getCalculationConfiguration ()
            PaymentRecord.Random currentConfig |> PaymentViewModel |> trades.Add
            )

    let removeTrade = SimpleCommand(fun trade -> trades.Remove (trade :?> PaymentViewModel) |> ignore)
    let clearTrades = SimpleCommand(fun _ -> trades.Clear () )
    (* options commands *)


    let calculateFunO _ = do
            options |> Seq.iter(fun option -> option.Calculate(getDataConfiguration (), getCalculationConfiguration ()))
            refreshSummary()

    let calculateO = SimpleCommand calculateFunO

    let addOption = SimpleCommand(fun _ -> 
            let currentConfig = getCalculationConfiguration ()
            OptionRecord.Random currentConfig |> OptionViewModel |> options.Add
            )

    let removeOption = SimpleCommand(fun option -> options.Remove (option :?> OptionViewModel) |> ignore)
    let clearOptions = SimpleCommand(fun _ -> options.Clear () )
    (* charting *)
    
    let chartSeries = SeriesCollection()

    let predefinedChartFunctions = [| (fun x -> sin x); (fun x -> x); (fun x -> x*x) |] 

    let addChartSeriesFun _ = do
                let ls = LineSeries()
                let multiplier = System.Random().NextDouble()
                let mapFun = predefinedChartFunctions.[ System.Random().Next(predefinedChartFunctions.Length) ]
                ls.Title <- sprintf "Test series %0.2f" multiplier
                let series = seq { for i in 1 .. 100 do yield (0.01 * multiplier * double i) }
                ls.Values <- ChartValues<float> (Seq.map mapFun series)
                chartSeries.Add(ls)

    let addChartSeries = SimpleCommand addChartSeriesFun

    (* add a few series for a good measure *)
    do
        addChartSeriesFun ()
        addChartSeriesFun ()

    (* market data commands *)
    let addMarketDataRecord = SimpleCommand (fun _ -> data.Add(ConfigurationViewModel { Key = ""; Value = "" }))
    let removeMarketDataRecord = SimpleCommand (fun record -> data.Remove(record :?> ConfigurationViewModel) |> ignore)
    let clearMarketDataRecord = SimpleCommand (fun _ -> data.Clear ())

    (* calculation parameters commands *)
    let addCalcParameterRecord = SimpleCommand (fun _ -> calculationParameters.Add(ConfigurationViewModel { Key = ""; Value = "" }))
    let removeCalcParameterRecord = SimpleCommand (fun record -> calculationParameters.Remove(record :?> ConfigurationViewModel) |> ignore)
    let clearCalcParameterRecord = SimpleCommand (fun _ -> calculationParameters.Clear ())
    let recalculate _ = 
        calculateFun()
        calculateFunO()
    (* automatically update summary when dependency data changes (entries added/removed)  *)
    do
        trades.CollectionChanged.Add calculateFun
        options.CollectionChanged.Add calculateFunO
        data.CollectionChanged.Add recalculate
        calculationParameters.CollectionChanged.Add recalculate

    (* commands *)
    member this.AddTrade = addTrade 
    member this.RemoveTrade = removeTrade
    member this.ClearTrades = clearTrades
    member this.Calculate = calculate

    member this.AddOption = addOption
    member this.RemoveOption = removeOption
    member this.ClearOptions = clearOptions
    member this.CalculateO = calculateO

    member this.AddMarketData = addMarketDataRecord
    member this.RemoveMarketData = removeMarketDataRecord
    member this.ClearMarketData = clearMarketDataRecord
    
    member this.AddCalcParameter = addCalcParameterRecord 
    member this.RemoveCalcParameter = removeCalcParameterRecord 
    member this.ClearCalcParameter = clearCalcParameterRecord 


    (* data fields *)
    member this.Trades = trades
    member this.Options = options
    member this.Data = data
    member this.CalculationParameters = calculationParameters
    member this.Summary = summary

    (* charting *)

    member this.ChartSeries = chartSeries
    member this.AddChartSeries = addChartSeries