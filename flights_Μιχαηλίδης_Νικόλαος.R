#Εργασία 1
########################################################3
#Κάτω από κάθε ερώτηση να τοποθετήσετε το κώδικα-απάντηση της αντίστοιχης ερώτησης
#Μπορείτε για κάθε απάντηση να χρησιμοποιήσετε οποιοδήποτε μοτίβο κώδικα έχετε διδαχθεί
#An den emfanizontai sosta ta ellinika epilegetai apo to menu tools->global options->code->saving->default code encoding->utf-8
#epeita epilegetai apply kleinete to arxeio kai to ksanaanoigete

#Να υπολογίσετε και να εμφανίσετε τις απαντήσεις για κάθε ένα από τα παρακάτω ερωτήματα

#Ερώτηση 1:να βρείτε (αν υπάρχουν) και να εμφανίσετε το πλήθος των κενών γραμμών σε κάθε στήλη του dataset
colSums(is.na(DelayedFlights))
#Ερώτηση 2:να υπολογίσετε και να εμφανίσετε ποια ημέρα σε ποιον μήνα σημειώθηκαν οι περισσότερες καθυστερήσεις πτήσεων
f<-DelayedFlights
f<-filter(f,f$ArrTime>f$CRSArrTime)
f<-filter(f,f$ArrTime>f$CRSArrTime)
f<-group_by(f,f$Month,f$DayofMonth)
f<-tally(f)
which.max(f$n)
#Ερώτηση 3: να υπολογίσετε και να εμφανίσετε τον ημερήσιο μέσο όρο καθυστερήσεων για καθέναν από τους θερινούς μήνες του 2008
f%>%
filter(Month>=6 & Month<=8, ArrDelay>=0)%>%
group_by(Month)%>%
  tally()%>%
  mutate(AvgDailyDelays=c(185394/30,167105/31,145598/31))

#Ερώτηση 4: να υπολογίσετε και να εμφανίσετε το όνομα της αεροπορικής εταιρίας που είχε το μεγαλύτερο πλήθος κωδικών ακύρωσης τύπου Β
f<-DelayedFlights
f%>%
  filter(CancellationCode=="B")%>%
  group_by(UniqueCarrier)%>%
  tally()%>%
  arrange(desc(n))%>%
  slice(1)
#Ερώτηση 5: να βρείτε τους κωδικούς των πτήσεων με τον μεγαλύτερο αριθμό καθυστερήσεων
f%>%
  filter(ArrDelay>0)%>%
  group_by(FlightNum)%>%
  tally()%>%
  arrange(desc(n))
rm(f)

#Ερώτηση 6: να βρείτε και να υπολογίσετε το όνομα του μεγαλύτερου σε απόσταση προορισμού με τις περισσότερες καθυστερήσεις
DelayedFlights%>%
  filter(ArrDelay>0, Distance==max(Distance))%>%
  select(Dest,Distance,ArrDelay)%>%
  group_by(Dest)%>%
  tally()%>%
  arrange(desc(n))

#Ερώτηση 7: να βρείτε και να εμφανίσετε τους προορισμούς που είχαν την μεγαλύτερη καθυστέρηση (πτήσεις πουεκτελέστηκαν)
DelayedFlights%>%
  filter(ArrDelay>0 & Cancelled==0)%>%
  select(Dest,ArrDelay,Cancelled)%>%
  group_by(Dest)%>%
  arrange(desc(ArrDelay))%>%
  tally()%>%
  arrange(desc(n))
  
#Ερώτηση 8: να βρείτε και να εμφανίσετε το όνομα της αεροπορικής εταιρείας που είχε τις μεγαλύτερες καθυστερήσεις που οφείλονται σε καθυστερημένη άφιξη αεροσκαφών
DelayedFlights%>%
  filter(ArrDelay>0 & LateAircraftDelay>0)%>%
  select(UniqueCarrier,ArrDelay,LateAircraftDelay)%>%
  group_by(UniqueCarrier)%>%
  tally()%>%
  arrange(desc(n))%>%
  slice(1)
#Ερώτηση 9: να υποlογίσετε πόσες ακυρώσεις πτήσεων τύπου Α σημειώθηκαν την 13η ημέρα κάθε μήνα
DelayedFlights%>%
  filter(DayofMonth==13 & CancellationCode=="A")%>%
  select(Month,DayofMonth,CancellationCode)%>%
  group_by(Month)%>%
  tally()

#Ερώτηση 10: υπολογίσετε και να εμφανίσετε την μέση καθυστέρηση πτήσεων που εκτελέστηκαν από την 10η μέχρι την 23 Απριλίου 2008
DelayedFlights%>%
  filter(DayofMonth >=10 & DayofMonth<=23 & Month==4 & ArrDelay>0)%>%
  select(ArrDelay,DayofMonth,Month)%>%
  summarise(ArrDelay=sum(ArrDelay)/13) 
#Ερώτηση 11: να υπολογίσετε και να εμφανίσετε τον μήνα που σημειώθηκε η μεγαλύτερη καθυστέρηση που οφειλόταν σε έλεγχους ασφαλείας κατά τις ώρες 06.00-14.00
DelayedFlights%>%
  filter(SecurityDelay==0, DepTime>=600, DepTime<=1400, DepDelay>0)%>%
  select(SecurityDelay,DepTime,Month,DepDelay)%>%
  arrange(desc(DepDelay))%>%
  slice(1)
#Ερώτηση 12: να υπολογίσετε και να εμφανίσετε ποιος κωδικός πτήσης(αριθμός πτήσης) είχε το πρώτο δεκαήμερο του Νοεμβρίου του 2008 την μεγαλύτερη προ του αναμενόμενου χρόνου άφιξη στον προορισμό της
DelayedFlights%>%
  filter(DayofMonth>=1 & DayofMonth<=10 & Month==11 & ArrDelay<0)%>%
  select(FlightNum,DayofMonth,Month,ArrDelay)%>%
  arrange(ArrDelay)%>%
  slice(1)
#Ερώτηση 13: να υπολογίσετε και να εμφανίσετε ποιο αεροδρόμιο (τοποθεσία αναχώρησης) είχε το δεύτερο δεκαήμερο του Αυγούστου 2018 τις περισσότερες πτήσεις με καθυστέρηση(αναχωρίσεων) μεγαλύτερη από μισή ώρα που οφείλονται στους αερομεταφορείς
   DelayedFlights%>%
  filter(Month==8,DayofMonth>=11,DayofMonth<=21,DepDelay>=30,CarrierDelay>0)%>%
  select(Origin,Month,DayofMonth,DepDelay,CarrierDelay)%>%
  group_by(Origin)%>%
  tally()%>%
  arrange(desc(n))
  

#Ερώτηση 14: να βρείτε και να εμφανίσετε τις πτήσεις που εκτράπηκαν από την πορεία τους αλλά ολοκληρώθηκαν καθώς και τον συνολικό χρόνο που απαιτήθηκε
DelayedFlights%>%
  filter(Diverted==1,Cancelled==0,ArrDelay!=0)%>%
  select(Diverted,Cancelled,CRSArrTime,DepTime)
  
  
  
#Ερώτηση 15: ποιος μήνας είχε την μεγαλύτερη τυπική απόκλιση σε καθυστερήσεις ("πιο απρόβλεπτος μήνας"). Ως απόκλιση να θεωρηθεί η διαφορά ανάμεσα στον προγραμματισμένο και τον πραγματικό χρόνο εκτέλεσης της πτήσης
DelayedFlights%>%
 select(CRSDepTime,DepTime,Month)%>%
  group_by(Month)%>%
  mutate(DepTime - CRSDepTime)%>%
  arrange(DepTime - CRSDepTime)
  
  #rename("DepTime - CRSDepTime" = "Apoklisi")
  #sd(DepTime - CRSDepTime, na.rm = TRUE)
  #tally()%>%
  #n%>%
 




