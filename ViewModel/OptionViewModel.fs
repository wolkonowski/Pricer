namespace ViewModel
 
//Representation of a Payment to the UI
type OptionViewModel(input : OptionRecord) = 
    inherit ViewModelBase()
    let types = ["European Put";"European Call"]
    let mutable userInput = input
    let mutable value : Money option = None
    let mutable delta : float option = None
    member this.OptionName 
        with get() = userInput.OptionName
        and set(x) = 
            userInput <- {userInput with OptionName = x }
            base.Notify("OptionName")
    member this.OptionType 
        with get() = userInput.OptionType
        and set(x) = 
            userInput <- {userInput with OptionType = x }
            base.Notify("OptionType")
    member this.Expiry 
        with get() = userInput.Expiry
        and set(x) = 
            userInput <- {userInput with Expiry = x }
            base.Notify("Expiry")

    member this.Currency 
        with get() = userInput.Currency
        and set(x) = 
            userInput <- {userInput with Currency = x }
            base.Notify("Currency")

    member this.Principal 
        with get() = userInput.Principal
        and set(x) = 
            userInput <- {userInput with Principal = x }
            base.Notify("Principal")

    member this.Amount 
           with get() = userInput.Amount
           and set(x) = 
               userInput <- {userInput with Amount = x }
               base.Notify("Amount")
    member this.Value
        with get() = value
        and set(x) = 
            value <- x
            base.Notify("Value")
    member this.Delta
        with get() = delta
        and set(x) = 
            delta <- x
            base.Notify("Delta")
    member this.Stock
        with get() = userInput.Stock
        and set(x) = 
                userInput <- {userInput with Stock = x}
                base.Notify("Stock")
    member this.Types = types
    // Invoke the valuation based on user input
    member this.Calculate(data : DataConfiguration, calculationParameters : CalculationConfiguration) = 
        System.Console.WriteLine(this.OptionType)
        //capture inputs
        let optionInputs : OptionValuationInputs = 
            {
                Option = 
                         {
                             OptionName = this.OptionName
                             OptionType = this.OptionType
                             Stock =      this.Stock
                             Expiry    =  this.Expiry
                             Currency  =  this.Currency
                             Principal =  this.Principal
                             Amount = this.Amount
                         }
                Data = data
                CalculationsParameters = calculationParameters
            }
        //calculate
        let calc = OptionValuationModel(optionInputs).Calculate()

        //present to the user
        this.Value <- Option.Some (calc.M)
        this.Delta <- Option.Some (calc.Delta)
        