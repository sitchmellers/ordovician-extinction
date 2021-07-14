
#################################################################################################################
#' Generate a soil profile for use in an erosional or depositional site
#'
#' @param layers A numeric vector of soil layers, not measured in units of depth.
#' @param SOC An optional numeric vector of SOC at depths corresponding to layers.
#' @param bulk_density A numeric vector of soil bulk density at depths corresponding to layers. Optional if SOC is present.
#' @param weight_fraction_c A numeric vector of the fraction of soil weight that carbon represents at depths corresponding to layers. Optional if SOC is present.
#' @param layer_range A numeric vector of the range of layers desired in the output. Defaults to 1:length(layers)
#' @param erosion_rate_mt A scalar indicating the erosion rate in units of meters/time.
#' @param interpolate A boolean indicating whether the user would like gen_profile_SOC to interpolate SOC values out to the depth given in layer_range.
#' @param calculate A boolean indiating whether the user would like gen_profile_SOC to calculate SOC from weight_fraction_c and bulk_density. Should be FALSE if SOC is supplied.
#' @param ero_accel A numeric vector which gives the factor by which the erosion rate should be scaled by for each time step.
#' @return A data frame of n rows and 6 columns where n is equal to layer_range and represents the number of layers in the profile to be eroded. Columns are layers, SOC, depth, k, weight fraction of C, and bulk density

#' @export
#' @import ggplot2
#' @importFrom minpack.lm nlsLM
#' @import crayon
#' @importFrom reshape melt

gen_profile_SOC <- function(layers, SOC, bulk_density, weight_fraction_c,
                            layer_range, erosion_rate_mt, min_age, max_age, interpolate=c(TRUE,FALSE),
                            calculate=c(TRUE, FALSE), ero_accel=rep(1, length(layers))){

  gen_k<-function(wfc, l, mxl, mna, mxa, interpolate=TRUE){#self-contained function for finding k. Here we use our min and max ages.
    k_init<-log(1/mna) #first K is log(1/ minimum age)
    k_final<-log(1/mxa)#last K is log(1/ maximum age)
    df<-data.frame(wfc, l) #holder data frame for computing k
    if(interpolate==TRUE){#start of interpolate clause
      fit<-nlsLM(weight_fraction_c~b*exp(-k*layers), start = list(b=.001,k=0.05), data=df)#draw line of best fit with starter parameters
      #the above model should give us an interpolation for weight fraction c

      print('K Model Info:')
      print(fit)

      new_cfrac <- predict(fit, newdata=data.frame(layers=1:mxl))#call the weight fraction c predict model
      kvec<-rep(NA, length(new_cfrac))
      #we then use those estimated weight fraction Cs to estimate K
      for (i in 2:(length(new_cfrac)-1)){#for everything in between the known start and ends of k, use weight fraction C to calculate k
        kvec[i]<-k_final+((new_cfrac[i]-tail(new_cfrac,1))/(new_cfrac[1]-tail(new_cfrac, 1)))*(k_init-k_final)
      }

      kvec[1]<-k_init
      kvec[length(kvec)]<-k_final#bookends of the k vector
    }#end of interpolate clause

    else{#if interpolate == FALSE here, meaning the length of submitted data is the total depth we're going to estimate
      kvec<-rep(NA, length(wfc))

      for (i in 2:(length(wfc)-1)){
        kvec[i]<-k_final+((wfc[i]-tail(wfc,1))/(wfc[1]-tail(wfc, 1)))*(k_init-k_final)
      }
      kvec[1]<-k_init
      kvec[length(kvec)]<-k_final#same logic as above without modeling weight fraction c
    }

    lnk<-log(-kvec) #finally, we take the natural log of negative k for ln k that we'll use in equations

    df2<-data.frame('layers'=1:mxl,'k'=kvec) #making a quick data frame for plotting

    kplot<-ggplot(data=df2, aes(x=k, y=layers))+
      geom_line(color='darkgreen')+
      labs(x='k', y='Layer')+
      scale_y_reverse()+
      ggtitle('k Extrapolation')+
      theme_grey()+
      theme(plot.title=element_text(hjust = 0.5))
    print(kplot)#plot k

    newdf2<-data.frame('layers'=1:mxl,'ln(-k)'=lnk)#making a quick data frame for plotting

    lnkplot<-ggplot(data=newdf2, aes(x=lnk, y=layers))+
      geom_line(color='darkblue')+
      labs(x='ln(-k)', y='Layer')+
      scale_y_reverse()+
      ggtitle('ln(-k) Extrapolation')+
      theme_grey()+
      theme(plot.title=element_text(hjust = 0.5))

    print(lnkplot)#plot ln(k)

    return(lnk)#return ln(k) as a vector at the end of the function
  }#end of the generate k function definition

  ifelse(calculate==TRUE,
    {
      erosionvec<-ero_accel*layers*erosion_rate_mt #a vector for depth, erosion rate times the number of layers, times any acceleration.
      #this is the short vector of depth, before we extrapolate anything
      df<-data.frame(layers, bulk_density, weight_fraction_c)
      df$depth<-erosionvec#putting the erosion vector into our dataframe after we compute it.
      df$SOC<-df$bulk_density*df$weight_fraction_c*1000*erosion_rate_mt#calculating SOC from Bulk Density and Weight Fraction of C.
      df<-df[, c('layers', 'SOC', 'depth')] #Keep all rows, but only keep layers, SOC and Depth for now.
      age<-c(min_age, rep(NA, length(layers)-2), max_age)#starter vector for Age. This gives the minimum age, a space of NAs, and then the max age.
      callargs<-data.frame(layers, bulk_density, weight_fraction_c, ero_accel, age)#shaping everything into the call args, for use later.

      k<-gen_k(weight_fraction_c, layers, max(layer_range), min_age, max_age, interpolate=TRUE)#use the function we just defined to give us our ln(k)
    },
    #end of if calculate==TRUE clause
    #start of if calculate==FALSE clause
    { erosionvec<-ero_accel*erosion_rate_mt*layers
      df<-data.frame(layers, SOC, depth, k)#we don't have to calculate SOC here since we already have it if calculate==FALSE
      df$depth<-erosionvec
      age<-c(min_age, rep(NA, length(layers)-2), max_age)
      callargs<-data.frame(layers, SOC, ero_accel, age)#call args for later use.
      k<-gen_k(weight_fraction_c, layers, max(layer_range), min_age, max_age, interpolate=TRUE)#use the function we just defined to give us our ln(k)

    }
  )

  ifelse(interpolate==TRUE,
    {
      fit<-minpack.lm::nlsLM(weight_fraction_c~b*exp(-k*layers), start = list(b=.001,k=0.05), data=df)#modeling weight fraction c as above

      new_cfrac <- predict(fit, newdata=data.frame(layers=1:max(layer_range))) #now we're generating a weight fraction c for the entire span of data, our to our eventual layer range

      fit<-minpack.lm::nlsLM(SOC~b*exp(-k*depth), start = list(b=.01,k=0.005), data=df)

      #get me a list of lengths for which each erosion acceleration is applied, then expand the vector out by those values
      layer_inc<-c()

      for (i in 2:length(layers)){
        layer_inc[i]<-layers[i]-layers[i-1]
      }
      layer_inc[1]<-layers[1]#layer_inc is giving us the distance between each layer, in layers
      #layer_inc = layer_inc*(1/erosion_rate_mt)

      erosionvec<-c()#empty new erosion vector
      for (l in 1:(length(layer_inc)-1)){
        rate_section<-rep(ero_accel[l], layer_inc[l])#we repeat out the acceleration factors for the length of each layer increment
        erosionvec<-c(erosionvec, rate_section)
      }

      long_accel<-c(erosionvec, rep(tail(ero_accel,1), tail(layer_inc,1)))

      #multiply erosion acceleration by erosion rates to get thickness of each layer
      layer_thickness<-long_accel*erosion_rate_mt#we have the total acceleration rate for the total length, for each layer thickness
      #cumulative sum to get depth

      new_depth<-cumsum(layer_thickness[1:length(layer_range)])#this works in our example, comes out to .5m
      new_SOC<-predict(fit, newdata=data.frame(depth=new_depth))#using the SOC model we defined above to bring SOC out to the total depth
      print('SOC Model Info:')
      print(fit)
      newbd<-new_SOC/new_cfrac#reverse calculate for bulk density
      newdf<-data.frame('layers'=layer_range,'SOC'=new_SOC, 'depth'=new_depth, 'k'=k, 'wfc'=new_cfrac,'bd'=newbd)

      depthplot<-ggplot(data=newdf, ggplot2::aes(x=SOC, y=depth))+
        ggplot2::geom_line(color='darkgreen')+
        ggplot2::labs(x=bquote('SOC, (kg' *~C/~m^2*')') , y='Depth (m)')+
        ggplot2::scale_y_reverse()+
        ggplot2::ggtitle('SOC Depth Extrapolation')+
        ggplot2::theme_grey()+
        ggplot2::theme(plot.title=ggplot2::element_text(hjust = 0.5))
      print(depthplot)
  },#end of if interpolate==TRUE clause
  {#start of if interpolate==FALSE clause
    if (calculate==TRUE){#if interpolate==FALSE and calculate==TRUE:

      erosionvec<-ero_accel*erosion_rate_mt*layers

      df<-data.frame(layers, bulk_density, weight_fraction_c)
      df$depth<-erosionvec
      new_SOC<-df$bulk_density*df$weight_fraction_c*1000*erosionvec

      newdf<-data.frame('layers'=df$layers, 'SOC'=new_SOC, 'depth'=df$depth, 'ero_accel'=ero_accel)

      depthplot<-ggplot(data=newdf, ggplot2::aes(x=SOC, y=depth))+
        ggplot2::geom_line(color='darkgreen')+
        ggplot2::labs(x=bquote('SOC, (kg' *~C/~m^2*')'), y='Depth (m)')+
        ggplot2::scale_y_reverse()+
        ggplot2::ggtitle('SOC and Depth')+
        ggplot2::theme_grey()+
        ggplot2::theme(plot.title=ggplot2::element_text(hjust = 0.5))
      print(depthplot)
    }
    else {new_SOC <- df$SOC}
  newdf<-data.frame('layers'=df$layers, 'SOC'=new_SOC, 'depth'=df$depth, 'ero_accel'=ero_accel)
  }
  )
  returnobject<-list('Profile'=newdf, 'CallArgs'=callargs)
  class(returnobject) <-append(class(returnobject), 'SoilProfile')
  return(returnobject)#returning newdf, and callargs
}
