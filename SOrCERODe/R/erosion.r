#################################################################################################################
#' Simulate an eroding soil profile's soil carbon balance
#'
#' @param profile An object of class "SoilProfile" object generated by SOrCERO::gen_profile_SOC().
#' @param time A number representing the amount of time steps to simulate.
#' @param k A numeric vector representing the natural log of the oxidation first order rate constant, ln(-k), for each layer of the eroded profile.
#' @param I A numeric vector representing the SOC production, I, in units of kg/m^2/time for each layer of the eroded profile.
#' @param n_ox A numeric vector representing the oxidation mixing coefficient for each layer of the eroded profile.
#' @param n_prod A numeric vector representing the production mixing coefficient for each layer of the eroded profile.
#' @return A list of three parts. The first, "Erosion Process" is another list whose entries represent the erosional profile at a given timestep, starting with 0. The second object in the list, "Timestep Metrics", gives a dataframe of useful metrics with rows as time step. The third object holds the original soil profile object.

#' @export
#' @import ggplot2
#' @importFrom minpack.lm nlsLM
#' @import crayon
#' @importFrom reshape melt


erosion<-function(profile, time, k=profile$Profile$k, I=NULL, n_ox, n_prod){#actual function definition starts here
  timerset<-Sys.time()#timer init

  # if (length(k)+1<=nrow(profile$Profile)){
  #   rest<-rep(k[length(k)], nrow(profile$Profile)-length(k))
  #   k<-c(k, rest)
  # }#these lines fill in k or I to the correct length needed
  # if (length(I)+1<=nrow(profile$Profile)){
  #   rest<-rep(I[length(I)], nrow(profile$Profile)-length(I))
  #   I<-c(I, rest)
  # }
  if (length(n_prod)+1<=nrow(profile$Profile)){
    rest<-rep(n_prod[length(n_prod)], nrow(profile$Profile)-length(n_prod))
    n_prod<-c(n_prod, rest)
  }
  if (length(n_ox)+1<=nrow(profile$Profile)){
    rest<-rep(n_ox[length(n_ox)], nrow(profile$Profile)-length(n_ox))
    n_ox<-c(n_ox, rest)
  }
  ero_list<-list()#empty list
  ero_rate<-profile$Profile$depth[2]-profile$Profile$depth[1]#default erosion rate

  if(is.null(I)) {#if I wasn't supplied we can just generate it
    I<-profile$Profile$SOC[1:length(k)]*(exp(-k))
  }

#########################################################################################################################
  stepwise_erosion<-c()#empty vector for each erosion step here
  for (t in 1:time){
    if (t==1){
      time_df<-data.frame('time_int'= rep(t, nrow(profile$Profile)-1),'layers'= profile$Profile$layers[-1:0],
                          'new_layer'=rep(NA, nrow(profile$Profile)-1),
                          'orig_depth'=seq(profile$Profile$depth[2], profile$Profile$depth[nrow(profile$Profile)], ero_rate),
                          'new_depth'=profile$Profile$depth[0:-1]-ero_rate,'SOC'=rep(NA, nrow(profile$Profile)-1)
                          )#intializing the loop
      #we have to do a loop rather than and parallel functions because our values will update for each step based on the calculation in the last step.

      for (l in 1:nrow(time_df)){#we're going through the equation defined by Dr. Billings for the first time step now
        time_df$SOC[l]<-profile$Profile$SOC[l+1]-((n_ox[l+1]*profile$Profile$SOC[l+1])
                                        *exp(-k[l])+
                                          (1-n_ox[l+1])*
                                          profile$Profile$SOC[l+1]
                                        *exp(-k[l+1]))*
          1+n_prod[l+1]*I[l]+(1-n_prod[l+1])*I[l+1]
       #print(time_df$SOC[l])#this is good too
        ############################################################
      }

      time_df$new_layer<-profile$Profile$layers[1:nrow(time_df)]
      #stepwise_erosion[t]<-time_df$SOC[1]
      profile$Profile$time_int<-rep(0, nrow(profile$Profile))
      profile$Profile$new_layer<-profile$Profile$layers
      profile$Profile$new_depth<-profile$Profile$depth
      names(profile$Profile)[names(profile$Profile)=='depth']<-'orig_depth'
      #names(profile$Profile$depth)<-'orig_depth'

      ero_list[[1]]<-profile$Profile

      #cumulative_SOC_eroded<-cumulative_SOC_eroded+time_df$SOC[1]
      ero_list[[2]]<-time_df

    }
    else{#for timesteps after 1. Holder df will be updated to time df after the calculations have taken place
      holder_df<-data.frame('time_int'= rep(t, nrow(profile$Profile)-t),'layers'= profile$Profile$layers[-t:0],
                            'new_layer'=rep(NA, nrow(profile$Profile)-t),'orig_depth'=seq(time_df$orig_depth[2], profile$Profile$orig_depth[nrow(profile$Profile)], ero_rate),
                            'new_depth'=head(profile$Profile$orig_depth, -t), 'SOC'=rep(NA, nrow(profile$Profile)-t)
                            )
      #print('okay coach')
      for (l in 1:nrow(holder_df)){
        #########################
        # if(l==nrow(holder_df) & t==4){#this was for testing, might as well leave it just in case
        #   print(paste('t', t))
        #   print(paste('l',l))
        #   print(paste('previous layer', time_df$SOC[l+1]))
        #   print(paste('nox l+1',n_ox[l+1]))
        #   print(paste('k l', k[l+t]))
        #   print(paste('k l+1',k[l+1]))
        #   print(paste('nprod l+1', n_prod[l+1]))
        #   print(paste('I l', I[l]))
        #   print(paste('I l+1', I[l+1]))
        # }
        #################################
        holder_df$SOC[l]<-time_df$SOC[l+1]-((n_ox[l+t]#1 #going through the layers for timestep!=1
                                             *time_df$SOC[l+1])#2
                                            *exp(-k[l])+#3
                                              (1-n_ox[l+t])*
                                              time_df$SOC[l+1]
                                        *exp(-k[l+t]))*#4
                                        1+n_prod[l+t]*I[l]+
                                        (1-n_prod[l+1])*I[l+t]
       #print(nrow(holder_df))
        # if (t==2){
        #   print('1')
        #   print(n_ox[l+t])
        #   print('2')
        #   print(time_df$SOC[l+1])
        #   print('3')
        #   print(exp(-k[l]))
        #   print('4')
        #   print(exp(-k[l+t]))
        #   }



      }
      #stepwise_erosion[t]<-holder_df$SOC[1]
      holder_df$new_layer<-profile$Profile$layers[1:nrow(holder_df)]
      time_df<-holder_df
      #print('assignment')
      ero_list[[t+1]]<-time_df
    }

  }

  for (i in 1:time){
    stepwise_erosion[i]<-ero_list[[i]]$SOC[1] #The first layer is eroded during each timestep, so we put that into the total amount eroded
  }
  cumulative_eroded_SOC<-cumsum(stepwise_erosion) #cumulative sum of the above
  remaining_SOC<-c()
  for (i in 1:(length(ero_list)-1)){ #we make a vector for remaining SOC in each timestep by summarizing the SOC for every step
    remaining_SOC[i]<-sum(ero_list[[i+1]]$SOC)
  }
  net_SOC_loss<-profile$Profile$SOC[1:time]-stepwise_erosion #here we subtract the erosion from the original SOC to show what was lost
  original_SOC_remaining<-c()#define empty vector

  for (i in 1:length(profile$Profile$SOC)){
    original_SOC_remaining[i]<-sum(profile$Profile$SOC[(i+1):length(profile$Profile$SOC)])
  }#summing remaining SOC from the original DF at each time step to get what is remaining of the original content


  original_SOC_remaining<-original_SOC_remaining[1:time]#take the above and cut it off at max timestep
  cumul_preerosion_net_loss<-original_SOC_remaining-remaining_SOC#original SOC remaining minus TOTAL remaining soc. This piece is giving us the value of the carbon loss through microbial activity
  systemsourcesink<-cumsum(net_SOC_loss)+cumul_preerosion_net_loss  #cumulative sum of the net loss plus the cumulative pre-erosion net loss
  maxpossiblesourcesink<-systemsourcesink+cumulative_eroded_SOC
  timestep_metrics<-data.frame('Time Index'=1:time, 'Erosion Per Timestep'=stepwise_erosion,
                               'Cumulative Eroded SOC'=cumulative_eroded_SOC,
                               'Remaining SOC'=remaining_SOC, 'Net SOC Loss'=net_SOC_loss,
                               'Cumulative Net SOC Loss'=cumsum(net_SOC_loss),
                               'Original SOC Remaining'= original_SOC_remaining,
                               'Cumulative Pre-Erosion Net SOC Loss'= cumul_preerosion_net_loss,
                               'Cumulative Original SOC'=cumsum(profile$Profile$SOC)[1:time],
                               'System Source or Sink'=systemsourcesink,
                               'Maximum Source or Minimum Sink'=maxpossiblesourcesink,
                               check.names = FALSE)

  cat('System Source (+) or Sink (-) at Timestep', time, ":", crayon::green(systemsourcesink[time]), "kg/m^2\n")
  #print('')
  cat('Maximum Possible Source (+) or Minimum Sink (-) at Timestep', time,':', crayon::red(maxpossiblesourcesink[time]), 'kg/m^2\n')

  melted_graph_df1<-reshape::melt(timestep_metrics[c(1,4,7)], id='Time Index')#reshaping for plots
  #print(net_SOC_loss)
  #print(systemsourcesink)
  #print(stepwise_erosion)

  plt1<-ggplot2::ggplot(data=melted_graph_df1, ggplot2::aes(x=`Time Index`, y=value, color= variable))+
    ggplot2::geom_line(size=1.5)+
    ggplot2::ylab(label=bquote('SOC, kg C/~m^2'))+
    ggplot2::xlab('Time Index')+
    ggplot2::scale_color_manual(values=c('darkgreen','grey'))+
    ggplot2::theme(legend.position = 'bottom',legend.direction = 'vertical', legend.title = ggplot2::element_blank())+
    ggplot2::labs(title="Remaining Layers, Original SOC Content and Actual SOC Remaining")
  print(plt1)

  # plt2<-ggplot2::ggplot(data=timestep_metrics, ggplot2::aes(x=Time_Index))+
  #   geom_line(ggplot2::aes(y=Remaining_SOC, color='Net SOC Remaining'), size=2)+
  #   geom_line(ggplot2::aes(y=Original_SOC_Remaining, color='Original SOC Remaining'), size=2)+
  #   ggplot2::labs(x='Time Index', y=bquote('Total SOC Remaining, kg C/'~m^2), title=)
  # print(plt2)

  melted_graph_df2<-reshape::melt(timestep_metrics[c(1,10,11)], id='Time Index')
  #systemsource/since = with oxidation
  plt2<-ggplot2::ggplot(data=melted_graph_df2, ggplot2::aes(x=`Time Index`, y=value, color= variable))+
    ggplot2::geom_line(size=1.5)+
    ggplot2::ylab(label=bquote('SOC, kg C/~m^2'))+
    ggplot2::xlab('Time Index')+
    ggplot2::scale_color_manual(values=c('red','blue'),
                       labels=c('With Oxidation','Without Oxidation'))+
    ggplot2::theme(legend.position = 'bottom',legend.direction = 'vertical', legend.title = element_blank())+
    ggplot2::labs(title="Net SOC Change With and Without Oxidation")
  print(plt2)
#
#   plt3<-ggplot2::ggplot(data=timestep_metrics, ggplot2::aes(x=`Time Index`))+
#     geom_line(ggplot2::aes(y=maxpossiblesourcesink,color='Without Oxidation'), color='darkgreen', size=2)+
#     geom_line(ggplot2::aes(y=systemsourcesink, color='With Oxidation'), size=2)+
#     ggplot2::labs(x='Time Index', y=bquote('Net SOC Change, kg C/'~m^2), title="Net SOC Change With and Without Oxidation")
#
#   print(plt3)

  melted_graph_df3<-reshape::melt(timestep_metrics[c(1,3,9)], id='Time Index')

  plt3<-ggplot2::ggplot(data=melted_graph_df3, ggplot2::aes(x=`Time Index`, y=value, color= variable))+
    ggplot2::geom_line(size=1.5)+
    ggplot2::ylab(label=bquote('SOC, kg C/~m^2'))+
    ggplot2::xlab('Time Index')+
    ggplot2::scale_color_manual(values=c('darkgoldenrod1','darkslategray')
                       )+
    ggplot2::theme(legend.position = 'bottom',legend.direction = 'vertical', legend.title = element_blank())+
    ggplot2::labs(title="Eroded layers: Original SOC content and actual SOC eroded")
  print(plt3)

  # plt4<-ggplot2::ggplot(data=timestep_metrics, ggplot2::aes(x=`Time Index`))+
  #   ggplot2::geom_line(ggplot2::aes(y=`Cumulative Eroded SOC`, color='Cumulative Eroded SOC'), size=2)+
  #   ggplot2::geom_line(ggplot2::aes(y=`Cumulative Original SOC Eroded`, color='Cumulative Original SOC Eroded'), size=2)+
  #   ggplot2::labs(x='Time Index', y=bquote('Net SOC Change, kg C/'~m^2), title="Eroded layers: Original SOC content and actual SOC eroded")
  #
  # print(plt4)

  timerfinish<-Sys.time()

  cat('Total Runtime:', crayon::cyan(timerfinish-timerset))

  returnlist<-list('Erosion Process' = ero_list, 'Timestep Metrics'= timestep_metrics, 'Profile'=profile)
  class(returnlist) <-append(class(returnlist), 'ErosionProcess')

  return(returnlist)
}


# calhoun_erosion <- erosion(profile = calhoun_profile,
#                            n_prod = 0.5,
#                            n_ox = 0.5,
#                            time = 480)

