# Stable
def fcst_adj(df, target_colnames, new_percentages, target_items, source_items=None, exception_items=None):
    '''
    Function Name: fcst_adj
    Arguments: 
    - df: A pandas dataframe object which will be modified. Ideally, a forecast. 
    
    - target_colnames: A list of names corresponding to the columns whose values will
      be modified
      
    - new_percentages: A dictionary of percent values to adjust the target items to.
      The dictionary keys correspond to the target_colnames and the values correspond
      to the intended value of the target items divided by their respective sums. 
      
    - target_items: A dictionary of filter conditions to be made for the target items.
      The values of the key/value pairs in this dictionary should be lists, to allow
      for multiple conditions. For example, if the intent is to change the 23rd and
      24th days in December of 2021, the target_items argument should be:
      {'year':'[2021]','month':'[12]','day':[23,24]}. The function takes this
      dictionary and uses it to construct filter conditions with pandas.
      
    - source_items: Optional. A dictionary of filter conditions to be made for the
      source items. This argument is used when there is a need to move volume (either
      positive or negative) from specific rows to the target rows. If no source is
      specified, fcst_adj will assume the intended source to be the opposite of the
      target and will distribute accordingly. The structure of this argument is the same
      as target_items. 
      
    - exception_items: Optional. A dictionary or list of dictionaries of filter
      conditions to be made for the exception items. This argument is designed to be
      used in cases where a forecast is going through numerous iterations of fcst_adj
      and previously edited rows should not be altered any further. Exception_items
      are used as identifiers for rows which should not be used as a source, even when
      no particular source is provided. 
      
    Returns: 
    Returns a data frame with shape identical to df.
    
    Description: 
    A function for altering numerical values in a data frame (by design, a forecast)
    without altering the sum of those numerical columns in the data frame. 
    '''
    
    workingdf = df.copy()
    targetcond = ''
    sourcecond = ''
    
    for k, v in target_items.items():
        targetcond = targetcond+'(workingdf["{key}"].isin({val})) & '.format(key = k, val=v)
        
    targetcond = targetcond.rsplit('&',1)[0]
    negative_targetcond = '(~'+re.sub('\)','', targetcond, 1)+'))'
    
    ##################################################################################
    if exception_items is None: 
        ne_full = negative_targetcond
    else:
        if isinstance(exception_items, dict):
            exception_items = [exception_items]

        e_list = []
        ne_list = []

        for exception in exception_items:
            print(exception)
            exceptcond = ''
            for k, v in exception.items():
                exceptcond = exceptcond+'(workingdf["{key}"].isin({val})) & '.format(key=k, val=v)
            exceptcond = exceptcond.rsplit('&',1)[0]
            negative_exceptcond = '(~'+re.sub('\)','', exceptcond, 1)+'))'

            e_list.append(exceptcond)
            ne_list.append(negative_exceptcond)
        ne_full = '&'.join(ne_list)
    ###################################################################################
    if source_items is None: 
        sourcecond = negative_targetcond
    else: 
        for k, v in source_items.items():
            sourcecond = sourcecond+'(workingdf["{key}"].isin({val})) & '.format(key = k, val=v)
    
        sourcecond = sourcecond.rsplit('&',1)[0]
    #######################################################################
    #generate the condition strings
    sourcefactordict = {
        'colnames':[],
        'target factor':[],
        'sum delta':[]
    }
    gbdict = {}#this goes in the aggregation statement of the groupby
    for x in new_percentages.keys():
        gbdict[x] = 'sum'
    targb = df.groupby(list(target_items.keys()), dropna = False).agg(
        gbdict
    )
    
    targb = pd.DataFrame(targb)
    display(targb)
    targb.reset_index(inplace = True)
    
    for item in list(new_percentages.keys()):
        sourcefactordict['colnames'].append(item)
        before_sum = workingdf[eval(targetcond)][item].sum() #heres what the sum in the group is before the change
        after_sum = workingdf[item].sum() * new_percentages[item]#here's what it would be after the change
        sum_delta = before_sum-after_sum # we add this to the source to keep balance
        sum_factor = after_sum/before_sum #this one we apply to the target
        sourcefactordict['target factor'].append(sum_factor)
        sourcefactordict['sum delta'].append(sum_delta)
        
        workingdf.loc[eval(targetcond), item] = workingdf.loc[eval(targetcond), item]*sum_factor #the target has been changed now
        
        before_source_sum = workingdf[eval(sourcecond) & eval(negative_targetcond) & eval(ne_full)][item].sum()
        after_source_sum = before_source_sum+sum_delta
        source_sum_factor = after_source_sum/before_source_sum
        
        workingdf.loc[eval(sourcecond) & eval(negative_targetcond) & eval(ne_full), item] =  workingdf.loc[eval(sourcecond) & eval(negative_targetcond) & eval(ne_full), item]*source_sum_factor#now the source has been changed
    #the source cannot be shared with a target. Therefore, the source cond must have an anti targetcond appended to it. That way
    #we can never adjust a target as the source of another target.
    print(pd.DataFrame.from_dict(sourcefactordict))
    return workingdf