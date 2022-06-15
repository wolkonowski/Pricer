﻿namespace ViewModel.Gaussians
/// Some basic statistics functions in F#, including erfc, erfcinv, normcdf, normpdf, 
/// norminv, additiveCorrection, multiplicativeCorrection, a Box-Mueller RandomSampler
/// and a unitized type for a Gaussian distribution. 
///
/// Based on Ralf Herbrich's samples at http://blogs.technet.com/b/apg/archive/2008/04/05/trueskill-through-time.aspx

module Gaussians = 

    open System
    
    /// Compute the square of a unitized number
    let sqr (x:float<'u>) = x * x 

    /// Computes the complementary error function. This function is defined 
    /// by 2/sqrt(pi) * integral from x to infinity of exp (-t^2) dt
    let erfc x =
        if (Double.IsNegativeInfinity x) then 2.0
        elif (Double.IsPositiveInfinity x) then 0.0
        else
            let z = abs x
            let t = 1.0 / (1.0 + 0.5 * z) 
            let res = t * exp (-z * z - 1.26551223 + t * (1.00002368 + t * (0.37409196 + t * (0.09678418 + t * (-0.18628806 + t * (0.27886807 + t * (-1.13520398 + t * (1.48851587 + t * (-0.82215223 + t * 0.17087277))))))))) 
            if (x >= 0.0) then res else 2.0 - res

    /// Computes the inverse of the complementary error function
    let erfcinv y = 
        if (y < 0.0 || y > 2.0) then
            failwith "Inverse complementary function not defined outside [0,2]."
        elif y = 0.0 then Double.PositiveInfinity
        elif y = 2.0 then Double.NegativeInfinity
        else 
            let x = 
                if (y >= 0.0485 && y <= 1.9515) then
                    let q = y - 1.0 
                    let r = q * q 
                    (((((0.01370600482778535*r - 0.3051415712357203)*r + 1.524304069216834)*r - 3.057303267970988)*r + 2.710410832036097)*r - 0.8862269264526915) * q /
                    (((((-0.05319931523264068*r + 0.6311946752267222)*r - 2.432796560310728)*r + 4.175081992982483)*r - 3.320170388221430)*r + 1.0)
                else if (y < 0.0485) then
                    let q = sqrt (-2.0 * log (y / 2.0)) 
                    (((((0.005504751339936943*q + 0.2279687217114118)*q + 1.697592457770869)*q + 1.802933168781950)*q + -3.093354679843504)*q - 2.077595676404383) / 
                    ((((0.007784695709041462*q + 0.3224671290700398)*q + 2.445134137142996)*q + 3.754408661907416)*q + 1.0)
                else if (y > 1.9515) then
                    let q = sqrt (-2.0 * log (1.0 - y / 2.0)) 
                    (-(((((0.005504751339936943*q + 0.2279687217114118)*q + 1.697592457770869)*q + 1.802933168781950)*q + -3.093354679843504)*q - 2.077595676404383) / 
                     ((((0.007784695709041462*q + 0.3224671290700398)*q + 2.445134137142996)*q + 3.754408661907416)*q + 1.0))
                else 0.0
            let u = (erfc x - y) / (-2.0 / sqrt Math.PI * exp (-x * x)) 
            x - u / (1.0 + x * u)

    /// Computes the cummulative Gaussian distribution at a specified point of interest
    let normcdf t = 
        let sqrt2 = 1.4142135623730951 
        (erfc (-t / sqrt2)) / 2.0

    /// Computes the Gaussian density at a specified point of interest
    let normpdf (t:float) = 
        let invsqrt2pi = 0.398942280401433
        invsqrt2pi * exp (- (t * t / 2.0))
        
    /// Computes the inverse of the cummulative Gaussian distribution (quantile function) at a specified point of interest
    let norminv p = 
        let sqrt2 = 1.4142135623730951 
        (-sqrt2 * erfcinv (2.0 * p))

    /// Computes the additive correction (v) of a single-sided truncated Gaussian with unit variance
    let additiveCorrection t = 
        match normcdf t with
        | denom when denom < 2.222758749e-162   -> -t
        | denom                                 -> (normpdf t) / denom                             

    /// Computes the multiplicative correction (w) of a single-sided truncated Gaussian with unit variance
    let multiplicativeCorrection t =
        match normcdf t with
        | denom when denom < 2.222758749e-162   -> if (t < 0.0) then 1.0 else 0.0
        | denom                                 -> let vt = additiveCorrection t in vt * (vt + t)

    /// Computes the additive correction of a double-sided truncated Gaussian with unit variance
    let additiveCorrection0 t epsilon = 
        let v = abs t
        match normcdf (epsilon - v) - normcdf (-epsilon - v) with
        | denom when denom < 2.222758749e-162   -> if t < 0.0 then -t-epsilon else -t+epsilon
        | denom                                 -> let num = normpdf (-epsilon-v) - normpdf (epsilon-v) in if t < 0.0 then -num/denom else num/denom

    /// Computes the multiplicative correction of a double-sided truncated Gaussian with unit variance
    let multiplicativeCorrection0 t epsilon =
        let v = abs t
        match normcdf (epsilon - v) - normcdf (-epsilon - v) with
        | denom when denom < 2.222758749e-162   -> 1.0
        | denom                                 -> let vt = additiveCorrection0 v epsilon in vt*vt + ((epsilon-v) * normpdf (epsilon-v) - (-epsilon-v) * normpdf (-epsilon-v))/denom


    /// Computes a random sampler using the Box-Mueller formula
    type RandomSampler(seed:int) =
        /// The internal state of the sampler
        let sampler = System.Random (seed)
        let mutable buffered = false
        let mutable buffer = 0.0
        // Generate a new pair of standard Gaussian distributed variables using the Box-Mueller algorithm.
        let rec nextSample () = 
            let u = sampler.NextDouble () 
            let v = sampler.NextDouble () 
            if (u = 0.0 || v = 0.0) then
                nextSample ()
            else
                let x = sqrt (-2.0 * log (u)) 
                (x * sin (2.0 * Math.PI * v), x * cos (2.0 * Math.PI * v)) 

        /// Generate a new normal sample distributed according to the standard Gaussian distribution
        member __.Sample () =

            if buffered then
                buffered <- not buffered
                buffer
            else
                let (x,y) = nextSample () 
                buffered <- not buffered
                buffer <- y
                x    

    let globalSampler = RandomSampler(42)


    /// A unitized Gaussian distribution based on float numbers (struct type for memory efficency) 
    /// in exponential parameterisation. 
    [<Struct>]
    type Gaussian<[<Measure>] 'u>(precisionMean:float<1/'u>,precision:float<1/'u^2>) = 
        static member FromMeanAndVariance(mean:float<'u>, variance:float<'u^2>) = 
            Gaussian<'u>(mean/variance, 1.0 / variance)
        static member FromMeanAndDeviation(mean:float<'u>, standardDeviation:float<'u>) = 
            let sigma = standardDeviation*standardDeviation
            Gaussian<'u>.FromMeanAndVariance(mean, sigma)
        /// Precision times the mean of the Gaussian
        member __.PrecisionMean  = precisionMean
        /// Precision of the Gaussian
        member __.Precision = precision
        /// Mean of the Gaussian
        member this.Mu = precisionMean / precision
        /// Mean of the Gaussian
        member this.Mean = this.Mu
        /// Variance of the Gaussian
        member this.Variance = 1.0 / precision
        /// Standard deviation of the Gaussian
        member this.StandardDeviation = sqrt this.Variance
        /// Standard deviation of the Gaussian
        member this.Sigma = this.StandardDeviation

        /// Multiplies two Gaussians  
        static member (*) (a:Gaussian<'u>,b:Gaussian<'u>) = 
            Gaussian<'u> (a.PrecisionMean + b.PrecisionMean, a.Precision + b.Precision)
        /// Divides two Gaussians
        static member (/) (a:Gaussian<'u>,b:Gaussian<'u>) =
            Gaussian<'u> (a.PrecisionMean - b.PrecisionMean, a.Precision - b.Precision)
        /// Computes the absolute difference between two Gaussians
        static member AbsoluteDifference (a:Gaussian<'u>) (b:Gaussian<'u>) = 
            max (abs (a.PrecisionMean - b.PrecisionMean)) (sqrt (abs (a.Precision - b.Precision)))
            //max (abs (a.PrecisionMean - b.PrecisionMean)) (abs (a.Precision - b.Precision))
        /// Computes the absolute difference between two Gaussians
        static member (-) (a:Gaussian<'u>,b:Gaussian<'u>) = Gaussian<'u>.AbsoluteDifference a b
        /// Used for string serialisation
        override this.ToString () = (string this.Mu) + ";" + (string this.Variance) 
        /// Generate a sample of this Gaussian using the global sampler
        member this.Sample() = this.Mean + this.Sigma * globalSampler.Sample()
        /// Computes the log-normalisation factor when two normalised Gaussians gets multiplied
        static member LogProductNormalisation (a:Gaussian<'u>,b:Gaussian<'u>) =
            if a.Precision = 0.0<_> then 
                0.0
            elif b.Precision = 0.0<_> then
                0.0
            else
                let varSum = a.Variance + b.Variance
                let muDiff = a.Mean - b.Mean
                -0.91893853320467267 - log(float varSum)/2.0 - muDiff*muDiff/(2.0 * varSum)
        /// Computes the log-normalisation factor when two normalised Gaussians gets divided
        static member LogRatioNormalisation (a:Gaussian<'u>,b:Gaussian<'u>) =
            if a.Precision = 0.0<_> then 
                0.0
            elif b.Precision = 0.0<_> then
                0.0
            else
                let v2 = b.Variance
                let varDiff = v2 - a.Variance
                let muDiff = a.Mean - b.Mean
                if varDiff = 0.0<_> then
                    0.0
                else
                    log(float v2) + 0.91893853320467267 - log(float varDiff)/2.0 + muDiff*muDiff/(2.0 * varDiff)