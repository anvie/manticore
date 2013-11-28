//+------------------------------------------------------------------+
//|                                                ManticoreEA.mq4 |
//|                                                        Manticore |
//|                                        http://www.metaquotes.net |
//+------------------------------------------------------------------+
#property copyright "Manticore"
#property link      "http://www.metaquotes.net"

//--- input parameters
extern int       ExApa;
string fileName = "out.csv";
int fh;
int prevCandleTime;

//+------------------------------------------------------------------+
//| expert initialization function                                   |
//+------------------------------------------------------------------+
int init()
  {
//----

   fh = FileOpen(fileName, FILE_CSV|FILE_READ|FILE_WRITE, ",");
   
//----
   return(0);
  }
//+------------------------------------------------------------------+
//| expert deinitialization function                                 |
//+------------------------------------------------------------------+
int deinit()
  {
//----
   FileClose(fh);
//----
   return(0);
  }
//+------------------------------------------------------------------+
//| expert start function                                            |
//+------------------------------------------------------------------+
int start()
  {
//----
   int rv, lastProceed, counted, i;
   int currentTime;
   
   if(Bars<100 || IsTradeAllowed()==false)
      return;
   
   currentTime = TimeCurrent();
   
   if (prevCandleTime+300 > currentTime)
      return;
   
   prevCandleTime = currentTime;
   
   FileWrite(fh, TimeToStr(currentTime), Open[1], High[1], Low[1], Close[1], Volume[1]);
   
   //counted = IndicatorCounted();
   
   //FileWrite(fh, TimeToStr(TimeCurrent()), Open[1], High[1], Low[1], Close[1], Volume[1]);      
   
   //rv = FileWrite(fh, TimeToStr(TimeCurrent()), Open[1], High[1], Low[1], Close[1], Volume[1]);
      
   //Comment(TimeToStr(TimeCurrent()) + " last open-close: " + (Open[1]-Close[1]));
   //Comment("last low at " + TimeToStr(lastLowTime) + " : " + lastLow + ", last gap: " + lastGap);
   
   //if (Open[1]-Close[1] > 0.0006){
   //   Comment("yap: " + (Open[1] - Close[1]));
   //}
   
   /*
   while(i > 0){
      double open = Open[i];
      double close = Close[i];
      if (open-close > 0.0005){
         Print("yap: " + (open-close));
      }
      i--;
   }
   */
   
//----
   return(0);
  }
//+------------------------------------------------------------------+


