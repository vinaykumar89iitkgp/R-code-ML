classifier_class_gmatcol_number<-function(dataset,min,max,no_of_classes,class_width)
{
  CC<-vector(mode='list',length=nrow(dataset))
  upper_class_boundary=min+class_width
  x=1
  z=1
  print(z)
  
  while(x<=274)
  {
    while(upper_class_boundary<=max)
    {
      if(dataset[x,"gmat_tot"]<=upper_class_boundary)
      {
        CC[x]<-z
        
        #print(CC[x])
        x=x+1
        break
      }
      
      else
      {
        upper_class_boundary=upper_class_boundary+class_width
        z=z+1
      }
      
    }
    
  }
  dataset_new=mutate(dataset,gmatclass_num=dataset[,"gmat_tot"]*0)
  x=1
  while(x<=274)
  {
    dataset_new[x,"gmatclass_num"]=CC[x]
    x=x+1
  }
  return(dataset_new)
}