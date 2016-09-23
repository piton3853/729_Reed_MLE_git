#Fearon and Laitin Stata .do file

ifelse(F){
  /* .do File to produce Table 1 results from replication data set */
    
    use repdata ,clear
  
  /* Model #1 */
  logit onset warl gdpenl lpopl lmtnest ncontig Oil nwstate instab polity2l ethfrac relfrac ,nolog
  
  /* Model #2 */
  logit ethonset warl gdpenl lpopl lmtnest ncontig Oil nwstate instab polity2l ethfrac relfrac if second > .049999,nolog
  
  /* Model #3 */
  logit onset warl gdpenl lpopl lmtnest ncontig Oil nwstate instab anocl deml ethfrac relfrac ,nolog
  
  /* Model #4 */
  logit emponset empwarl empgdpenl emplpopl emplmtnest empncontig Oil nwstate instab empethfrac ,nolog
  
  /* Model #5 */
  logit cowonset cowwarl gdpenl lpopl lmtnest ncontig Oil nwstate instab anocl deml ethfrac relfrac ,nolog
  
}