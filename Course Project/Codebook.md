# Code Book

## Data
| Names | Description |
| ----- | ----------- |
| wholeData | A set of entire data |
| subData | A subset of mean and standard deviation data from wholeData |
| tidyData | An independent tidy data set with the average of each variable for each activity and each subject |
| TidyData.txt | A text file contains tidy data |

## Key Variables
| Variable names | Description |
| -------------- | ----------- |
| activity | Performed activities for measurement |
| subject | Subject ID |

## Summary of tidy data
  activity     subject     TimeBodyAccelerometerMeanX TimeBodyAccelerometerMeanY TimeBodyAccelerometerMeanZ TimeBodyAccelerometerStandardDeviationX
 WALKING           :30   Min.   : 1.0   Min.   :0.2216             Min.   :-0.040514          Min.   :-0.15251           Min.   :-0.9961                        
 WALKING_UPSTAIRS  :30   1st Qu.: 8.0   1st Qu.:0.2712             1st Qu.:-0.020022          1st Qu.:-0.11207           1st Qu.:-0.9799                        
 WALKING_DOWNSTAIRS:30   Median :15.5   Median :0.2770             Median :-0.017262          Median :-0.10819           Median :-0.7526                        
 SITTING           :30   Mean   :15.5   Mean   :0.2743             Mean   :-0.017876          Mean   :-0.10916           Mean   :-0.5577                        
 STANDING          :30   3rd Qu.:23.0   3rd Qu.:0.2800             3rd Qu.:-0.014936          3rd Qu.:-0.10443           3rd Qu.:-0.1984                        
 LAYING            :30   Max.   :30.0   Max.   :0.3015             Max.   :-0.001308          Max.   :-0.07538           Max.   : 0.6269                        
 TimeBodyAccelerometerStandardDeviationY TimeBodyAccelerometerStandardDeviationZ TimeGravityAccelerometerMeanX TimeGravityAccelerometerMeanY
 Min.   :-0.99024                        Min.   :-0.9877                         Min.   :-0.6800               Min.   :-0.47989             
 1st Qu.:-0.94205                        1st Qu.:-0.9498                         1st Qu.: 0.8376               1st Qu.:-0.23319             
 Median :-0.50897                        Median :-0.6518                         Median : 0.9208               Median :-0.12782             
 Mean   :-0.46046                        Mean   :-0.5756                         Mean   : 0.6975               Mean   :-0.01621             
 3rd Qu.:-0.03077                        3rd Qu.:-0.2306                         3rd Qu.: 0.9425               3rd Qu.: 0.08773             
 Max.   : 0.61694                        Max.   : 0.6090                         Max.   : 0.9745               Max.   : 0.95659             
 TimeGravityAccelerometerMeanZ TimeGravityAccelerometerStandardDeviationX TimeGravityAccelerometerStandardDeviationY TimeGravityAccelerometerStandardDeviationZ
 Min.   :-0.49509              Min.   :-0.9968                            Min.   :-0.9942                            Min.   :-0.9910                           
 1st Qu.:-0.11726              1st Qu.:-0.9825                            1st Qu.:-0.9711                            1st Qu.:-0.9605                           
 Median : 0.02384              Median :-0.9695                            Median :-0.9590                            Median :-0.9450                           
 Mean   : 0.07413              Mean   :-0.9638                            Mean   :-0.9524                            Mean   :-0.9364                           
 3rd Qu.: 0.14946              3rd Qu.:-0.9509                            3rd Qu.:-0.9370                            3rd Qu.:-0.9180                           
 Max.   : 0.95787              Max.   :-0.8296                            Max.   :-0.6436                            Max.   :-0.6102                           
 TimeBodyAccelerometerJerkMeanX TimeBodyAccelerometerJerkMeanY TimeBodyAccelerometerJerkMeanZ TimeBodyAccelerometerJerkStandardDeviationX
 Min.   :0.04269                Min.   :-0.0386872             Min.   :-0.067458              Min.   :-0.9946                            
 1st Qu.:0.07396                1st Qu.: 0.0004664             1st Qu.:-0.010601              1st Qu.:-0.9832                            
 Median :0.07640                Median : 0.0094698             Median :-0.003861              Median :-0.8104                            
 Mean   :0.07947                Mean   : 0.0075652             Mean   :-0.004953              Mean   :-0.5949                            
 3rd Qu.:0.08330                3rd Qu.: 0.0134008             3rd Qu.: 0.001958              3rd Qu.:-0.2233                            
 Max.   :0.13019                Max.   : 0.0568186             Max.   : 0.038053              Max.   : 0.5443                            
 TimeBodyAccelerometerJerkStandardDeviationY TimeBodyAccelerometerJerkStandardDeviationZ TimeBodyGyroscopeMeanX TimeBodyGyroscopeMeanY TimeBodyGyroscopeMeanZ
 Min.   :-0.9895                             Min.   :-0.99329                            Min.   :-0.20578       Min.   :-0.20421       Min.   :-0.07245      
 1st Qu.:-0.9724                             1st Qu.:-0.98266                            1st Qu.:-0.04712       1st Qu.:-0.08955       1st Qu.: 0.07475      
 Median :-0.7756                             Median :-0.88366                            Median :-0.02871       Median :-0.07318       Median : 0.08512      
 Mean   :-0.5654                             Mean   :-0.73596                            Mean   :-0.03244       Mean   :-0.07426       Mean   : 0.08744      
 3rd Qu.:-0.1483                             3rd Qu.:-0.51212                            3rd Qu.:-0.01676       3rd Qu.:-0.06113       3rd Qu.: 0.10177      
 Max.   : 0.3553                             Max.   : 0.03102                            Max.   : 0.19270       Max.   : 0.02747       Max.   : 0.17910      
 TimeBodyGyroscopeStandardDeviationX TimeBodyGyroscopeStandardDeviationY TimeBodyGyroscopeStandardDeviationZ TimeBodyGyroscopeJerkMeanX TimeBodyGyroscopeJerkMeanY
 Min.   :-0.9943                     Min.   :-0.9942                     Min.   :-0.9855                     Min.   :-0.15721           Min.   :-0.07681          
 1st Qu.:-0.9735                     1st Qu.:-0.9629                     1st Qu.:-0.9609                     1st Qu.:-0.10322           1st Qu.:-0.04552          
 Median :-0.7890                     Median :-0.8017                     Median :-0.8010                     Median :-0.09868           Median :-0.04112          
 Mean   :-0.6916                     Mean   :-0.6533                     Mean   :-0.6164                     Mean   :-0.09606           Mean   :-0.04269          
 3rd Qu.:-0.4414                     3rd Qu.:-0.4196                     3rd Qu.:-0.3106                     3rd Qu.:-0.09110           3rd Qu.:-0.03842          
 Max.   : 0.2677                     Max.   : 0.4765                     Max.   : 0.5649                     Max.   :-0.02209           Max.   :-0.01320          
 TimeBodyGyroscopeJerkMeanZ TimeBodyGyroscopeJerkStandardDeviationX TimeBodyGyroscopeJerkStandardDeviationY TimeBodyGyroscopeJerkStandardDeviationZ
 Min.   :-0.092500          Min.   :-0.9965                         Min.   :-0.9971                         Min.   :-0.9954                        
 1st Qu.:-0.061725          1st Qu.:-0.9800                         1st Qu.:-0.9832                         1st Qu.:-0.9848                        
 Median :-0.053430          Median :-0.8396                         Median :-0.8942                         Median :-0.8610                        
 Mean   :-0.054802          Mean   :-0.7036                         Mean   :-0.7636                         Mean   :-0.7096                        
 3rd Qu.:-0.048985          3rd Qu.:-0.4629                         3rd Qu.:-0.5861                         3rd Qu.:-0.4741                        
 Max.   :-0.006941          Max.   : 0.1791                         Max.   : 0.2959                         Max.   : 0.1932                        
 TimeBodyAccelerometerMagnitude-mean TimeBodyAccelerometerMagnitude-std TimeGravityAccelerometerMagnitude-mean TimeGravityAccelerometerMagnitude-std
 Min.   :-0.9865                     Min.   :-0.9865                    Min.   :-0.9865                        Min.   :-0.9865                      
 1st Qu.:-0.9573                     1st Qu.:-0.9430                    1st Qu.:-0.9573                        1st Qu.:-0.9430                      
 Median :-0.4829                     Median :-0.6074                    Median :-0.4829                        Median :-0.6074                      
 Mean   :-0.4973                     Mean   :-0.5439                    Mean   :-0.4973                        Mean   :-0.5439                      
 3rd Qu.:-0.0919                     3rd Qu.:-0.2090                    3rd Qu.:-0.0919                        3rd Qu.:-0.2090                      
 Max.   : 0.6446                     Max.   : 0.4284                    Max.   : 0.6446                        Max.   : 0.4284                      
 TimeBodyAccelerometerJerkMagnitude-mean TimeBodyAccelerometerJerkMagnitude-std TimeBodyGyroscopeMagnitude-mean TimeBodyGyroscopeMagnitude-std
 Min.   :-0.9928                         Min.   :-0.9946                        Min.   :-0.9807                 Min.   :-0.9814               
 1st Qu.:-0.9807                         1st Qu.:-0.9765                        1st Qu.:-0.9461                 1st Qu.:-0.9476               
 Median :-0.8168                         Median :-0.8014                        Median :-0.6551                 Median :-0.7420               
 Mean   :-0.6079                         Mean   :-0.5842                        Mean   :-0.5652                 Mean   :-0.6304               
 3rd Qu.:-0.2456                         3rd Qu.:-0.2173                        3rd Qu.:-0.2159                 3rd Qu.:-0.3602               
 Max.   : 0.4345                         Max.   : 0.4506                        Max.   : 0.4180                 Max.   : 0.3000               
 TimeBodyGyroscopeJerkMagnitude-mean TimeBodyGyroscopeJerkMagnitude-std FrequencyBodyAccelerometerMeanX FrequencyBodyAccelerometerMeanY FrequencyBodyAccelerometerMeanZ
 Min.   :-0.99732                    Min.   :-0.9977                    Min.   :-0.9952                 Min.   :-0.98903                Min.   :-0.9895                
 1st Qu.:-0.98515                    1st Qu.:-0.9805                    1st Qu.:-0.9787                 1st Qu.:-0.95361                1st Qu.:-0.9619                
 Median :-0.86479                    Median :-0.8809                    Median :-0.7691                 Median :-0.59498                Median :-0.7236                
 Mean   :-0.73637                    Mean   :-0.7550                    Mean   :-0.5758                 Mean   :-0.48873                Mean   :-0.6297                
 3rd Qu.:-0.51186                    3rd Qu.:-0.5767                    3rd Qu.:-0.2174                 3rd Qu.:-0.06341                3rd Qu.:-0.3183                
 Max.   : 0.08758                    Max.   : 0.2502                    Max.   : 0.5370                 Max.   : 0.52419                Max.   : 0.2807                
 FrequencyBodyAccelerometerStandardDeviationX FrequencyBodyAccelerometerStandardDeviationY FrequencyBodyAccelerometerStandardDeviationZ
 Min.   :-0.9966                              Min.   :-0.99068                             Min.   :-0.9872                             
 1st Qu.:-0.9820                              1st Qu.:-0.94042                             1st Qu.:-0.9459                             
 Median :-0.7470                              Median :-0.51338                             Median :-0.6441                             
 Mean   :-0.5522                              Mean   :-0.48148                             Mean   :-0.5824                             
 3rd Qu.:-0.1966                              3rd Qu.:-0.07913                             3rd Qu.:-0.2655                             
 Max.   : 0.6585                              Max.   : 0.56019                             Max.   : 0.6871                             
 FrequencyBodyAccelerometerJerkMeanX FrequencyBodyAccelerometerJerkMeanY FrequencyBodyAccelerometerJerkMeanZ FrequencyBodyAccelerometerJerkStandardDeviationX
 Min.   :-0.9946                     Min.   :-0.9894                     Min.   :-0.9920                     Min.   :-0.9951                                 
 1st Qu.:-0.9828                     1st Qu.:-0.9725                     1st Qu.:-0.9796                     1st Qu.:-0.9847                                 
 Median :-0.8126                     Median :-0.7817                     Median :-0.8707                     Median :-0.8254                                 
 Mean   :-0.6139                     Mean   :-0.5882                     Mean   :-0.7144                     Mean   :-0.6121                                 
 3rd Qu.:-0.2820                     3rd Qu.:-0.1963                     3rd Qu.:-0.4697                     3rd Qu.:-0.2475                                 
 Max.   : 0.4743                     Max.   : 0.2767                     Max.   : 0.1578                     Max.   : 0.4768                                 
 FrequencyBodyAccelerometerJerkStandardDeviationY FrequencyBodyAccelerometerJerkStandardDeviationZ FrequencyBodyGyroscopeMeanX FrequencyBodyGyroscopeMeanY
 Min.   :-0.9905                                  Min.   :-0.993108                                Min.   :-0.9931             Min.   :-0.9940            
 1st Qu.:-0.9737                                  1st Qu.:-0.983747                                1st Qu.:-0.9697             1st Qu.:-0.9700            
 Median :-0.7852                                  Median :-0.895121                                Median :-0.7300             Median :-0.8141            
 Mean   :-0.5707                                  Mean   :-0.756489                                Mean   :-0.6367             Mean   :-0.6767            
 3rd Qu.:-0.1685                                  3rd Qu.:-0.543787                                3rd Qu.:-0.3387             3rd Qu.:-0.4458            
 Max.   : 0.3498                                  Max.   :-0.006236                                Max.   : 0.4750             Max.   : 0.3288            
 FrequencyBodyGyroscopeMeanZ FrequencyBodyGyroscopeStandardDeviationX FrequencyBodyGyroscopeStandardDeviationY FrequencyBodyGyroscopeStandardDeviationZ
 Min.   :-0.9860             Min.   :-0.9947                          Min.   :-0.9944                          Min.   :-0.9867                         
 1st Qu.:-0.9624             1st Qu.:-0.9750                          1st Qu.:-0.9602                          1st Qu.:-0.9643                         
 Median :-0.7909             Median :-0.8086                          Median :-0.7964                          Median :-0.8224                         
 Mean   :-0.6044             Mean   :-0.7110                          Mean   :-0.6454                          Mean   :-0.6577                         
 3rd Qu.:-0.2635             3rd Qu.:-0.4813                          3rd Qu.:-0.4154                          3rd Qu.:-0.3916                         
 Max.   : 0.4924             Max.   : 0.1966                          Max.   : 0.6462                          Max.   : 0.5225                         
 FrequencyBodyAccelerometerMagnitude-mean FrequencyBodyAccelerometerMagnitude-std FrequencyBodyBodyAccelerometerJerkMagnitude-mean
 Min.   :-0.9868                          Min.   :-0.9876                         Min.   :-0.9940                                 
 1st Qu.:-0.9560                          1st Qu.:-0.9452                         1st Qu.:-0.9770                                 
 Median :-0.6703                          Median :-0.6513                         Median :-0.7940                                 
 Mean   :-0.5365                          Mean   :-0.6210                         Mean   :-0.5756                                 
 3rd Qu.:-0.1622                          3rd Qu.:-0.3654                         3rd Qu.:-0.1872                                 
 Max.   : 0.5866                          Max.   : 0.1787                         Max.   : 0.5384                                 
 FrequencyBodyBodyAccelerometerJerkMagnitude-std FrequencyBodyBodyGyroscopeMagnitude-mean FrequencyBodyBodyGyroscopeMagnitude-std
 Min.   :-0.9944                                 Min.   :-0.9865                          Min.   :-0.9815                        
 1st Qu.:-0.9752                                 1st Qu.:-0.9616                          1st Qu.:-0.9488                        
 Median :-0.8126                                 Median :-0.7657                          Median :-0.7727                        
 Mean   :-0.5992                                 Mean   :-0.6671                          Mean   :-0.6723                        
 3rd Qu.:-0.2668                                 3rd Qu.:-0.4087                          3rd Qu.:-0.4277                        
 Max.   : 0.3163                                 Max.   : 0.2040                          Max.   : 0.2367                        
 FrequencyBodyBodyGyroscopeJerkMagnitude-mean FrequencyBodyBodyGyroscopeJerkMagnitude-std
 Min.   :-0.9976                              Min.   :-0.9976                            
 1st Qu.:-0.9813                              1st Qu.:-0.9802                            
 Median :-0.8779                              Median :-0.8941                            
 Mean   :-0.7564                              Mean   :-0.7715                            
 3rd Qu.:-0.5831                              3rd Qu.:-0.6081                            
 Max.   : 0.1466                              Max.   : 0.2878                 