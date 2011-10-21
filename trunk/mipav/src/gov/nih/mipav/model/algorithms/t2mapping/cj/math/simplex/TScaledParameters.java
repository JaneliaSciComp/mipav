package gov.nih.mipav.model.algorithms.t2mapping.cj.math.simplex;

import java.lang.Math;

public class TScaledParameters
extends TParameters {

   // VARIABLE DECLARATIONS

   /** Internal Scaling: minimum value */
   private double ScaleMinVal;
   /** Internal Scaling: maximum value */
   private double ScaleMaxVal;
   /** Scaling Factor */
   private double ScaleFactor[];
   /** Scaling Factor for Fitness */
   private double FitnessScaleFactor;



   // CONSTRUCTORS

   /**
   Initialize new Project.
   @param
   <I>aScaleMin:</I> Internal scaling: Minimum<BR>
   <I>aScaleMax:</I> Internal scaling:Maximum<BR>
   <I>acParams:</I> Number of Parameters for new Project<BR>
   */
   public TScaledParameters (double aScaleMinVal, double aScaleMaxVal,
   int acParams){
      super (acParams);
      int cP = getcParameters();
      ScaleFactor = new double[cP + 1];
      changeScaling(aScaleMinVal, aScaleMaxVal);
   }


   /**
   Initialize new Project with default internal scaling settings:
   ScaleMin = 0;    ScaleMax = 100.
   @param
   <I>acParams:</I> Number of Parameters for new Project<BR>
   */
   public TScaledParameters (int acParams) {
      this (0, 100, acParams);
   }


   /**
   Initialize, but load old project file with name aParamFileName
   @param
   <I>aParamFileName:</I> Filename of <B>existing</B> Project.<BR>
   <I>aScaleMin:</I> Internal scaling: Minimum<BR>
   <I>aScaleMax:</I> Internal scaling:Maximum<BR>
   */
   public TScaledParameters (String aParamFileName,
   double aScaleMinVal, double aScaleMaxVal) {
      super (aParamFileName);

      int cP = getcParameters();
      ScaleFactor = new double[cP + 1];
      changeScaling(aScaleMinVal, aScaleMaxVal);
   }

   /**
   Initialize and load old project file with default internal
        scaling (0-100)
   @param
   <I>aParamFileName:</I> Filename of <B>existing</B> project.
   */
   public TScaledParameters (String aParamFileName) {
      this (aParamFileName, 0, 100);
   }


   // METHODS

   public String toString() {
      String S = super.toString();
      S +=
      "     TScaledParamters Class:\n" +
      "++++++++++++++++++++++++++++++++++++"  + "\n" +
      "ScaleMinVal          = " + ScaleMinVal + "\n" +
      "ScaleMaxVal          = " + ScaleMaxVal  + "\n" +
      "------------------------------------"  + "\n";

      return S;
   }

   /**
   Change internal Scaling to new limits.
   @param
   <I>aScaleMin:</I> Internal scaling: Minimum<BR>
   <I>aScaleMax:</I> Internal scaling:Maximum<BR>
   */
   public boolean changeScaling (double aScaleMin, double aScaleMax) {
           if (aScaleMin < aScaleMax) {

              ScaleMinVal = aScaleMin;
              ScaleMaxVal = aScaleMax;

              // Set parameter Scaling Factors
              for (int i = 1; (i <= getcParameters()); i++){
                 double MinVal = getMinimum(i);
                 double MaxVal = getMaximum(i);
                 double lo;
                 double li;
                 lo = (MaxVal - MinVal);
                 li = (ScaleMaxVal - ScaleMinVal);
                 ScaleFactor[i] = (lo/li);
              }

              // Set Fitness Scaling Factor
              double MinVal = getFitnessMinimum();
              double MaxVal = getFitnessMaximum();
              double lo;
              double li;
              lo = (MaxVal - MinVal);
              li = (ScaleMaxVal - ScaleMinVal);
              FitnessScaleFactor = (lo/li);
              return true;
           }
           else
              return false;
        }


   /**
   Set minimal value of Parameter
   @param
   <I>ParamIdx</I> : Number of Parameter <BR>
   <I>Value</I> : minimal Value
   */
   public void setMinimum (int ParamIdx, double Value) {
          super.setMinimum (ParamIdx, Value);
          changeScaling(ScaleMinVal, ScaleMaxVal);
   }


   /**
   Set maximal value of Parameter
   @param
   <I>ParamIdx</I> : Number of Parameter <BR>
   <I>Value</I> : maximal Value
   */
   public void setMaximum (int ParamIdx, double Value) {
          super.setMaximum (ParamIdx, Value);
          changeScaling(ScaleMinVal, ScaleMaxVal);
   }



   /**
   Set minimal value of Fitness
   @param
   <I>Value</I> : minimal Value
   */
   public void setFitnessMinimum (double Value) {
          super.setFitnessMinimum (Value);
          changeScaling(ScaleMinVal, ScaleMaxVal);
   }



   /**
   Set maximal value of Fitness
   @param
   <I>Value</I> : maximal Value
   */
   public void setFitnessMaximum (double Value) {
          super.setFitnessMaximum (Value);
          changeScaling(ScaleMinVal, ScaleMaxVal);
   }




   /**
   Get lower limit of internal scaling
   */
   public double getScaleMin () {
      return ScaleMinVal;
   }


   /**
   Get upper limit of internal scaling
   */
   public double getScaleMax () {
      return ScaleMaxVal;
   }


   // GET Values


   /**
   Get Fitness Value of last Parameterset rescaled to internal scaling.
   */
   public double getScaledLastFitness() {
      return
      Val2ScaledVal (getFitnessMinimum(), FitnessScaleFactor, getLastFitness());
   }



   /**
   Get Fitness Value of Parameterset <I>ParSetNr</I>, rescaled
   to internal scaling.
   @param
   <I>ParSetNr:</I> Number of Parameterset (1-last)
   */
   public double getScaledFitness (int ParSetNr) {
      return
      Val2ScaledVal(getFitnessMinimum(), FitnessScaleFactor, getFitness(ParSetNr));
   }


   /**
   Get Value of Parameter <I>ParamIdx</I> in last Parameterset, rescaled
   to internal scaling.
   @param
   <I>ParamIdx:</I> Index of Parameter (1-last)
   */
   public double getScaledLastParSetValue(int ParamIdx) {
      return
      Val2ScaledVal(getMinimum(ParamIdx), ScaleFactor[ParamIdx],
      getLastParSetValue(ParamIdx));
   }


   /**
   Get Value of Parameter <I>ParamIdx</I> in Parameterset <I>ParSetNr</I>,
   rescaled to internal scaling.
   @param
   <I>ParSetNr:</I> Number of Parameterset in History (1-last)<BR>
   <I>ParamIdx:</I> Index of Parameter (1-last)
   */
   public double getScaledParSetValue(int ParSetNr, int ParamIdx) {
      return
      Val2ScaledVal(getMinimum(ParamIdx), ScaleFactor[ParamIdx],
      getParSetValue(ParSetNr, ParamIdx));
   }


   // SET Values


   /**
   Set Fitness Value of last Parameter set to <I>Value</I> in internal
   scale. (Will be transformed to "real" values, before entering.)
   @param
   <I>Value:</I> Value of Fitness in internal scaling.
   */
   public boolean setScaledLastFitness (double Value) {
           boolean WriteOK = false;
           if ( (Value >= ScaleMinVal) & (Value <= ScaleMaxVal) ) {
              WriteOK = setLastFitness(ScaledVal2Val(getFitnessMinimum(),
              FitnessScaleFactor,Value));
           }
           return WriteOK;
          }


   /**
   Set Value <I>Value</I> for Parameter <I>ParamIdx</I> of last Parameterset,
   in internal scale. (Will be transformed to "real" values, before entering.)
   @param
   <I>ParamIdx:</I> Index of Parameter (1-last)<BR>
   <I>Value:</I> Value of Parameter in internal scaling.
   */
   public boolean setScaledLastParSetValue(int ParamIdx, double Value) {
           boolean WriteOK = false;
           if ( (Value >= ScaleMinVal) & (Value <= ScaleMaxVal) ) {
              WriteOK = setLastParSetValue(ParamIdx,
		    ScaledVal2Val(getMinimum(ParamIdx), ScaleFactor[ParamIdx],Value));
           }
           return WriteOK;
   }


   // private SCALING FUNCTIONS

   /**
   Calculate scaling from external to internal scale
   @param
   <I>MinVal:</I> minimum Value of Parameter <BR>
   <I>ScaleFactor:</I> ScaleFactor of Parameter <BR>
   <I>ExtValue:</I> "external" Value to be rescaled <BR>
   */
   private double Val2ScaledVal(double MinVal, double ScaleFactor,
   double ExtValue) {
           double lv;
           lv = (ExtValue - MinVal);
           return (ScaleMinVal + lv * (1/ScaleFactor));
   }


   /**
   Calculate scaling from internal to external scale
   @param
   <I>MinVal:</I> minimum Value of Parameter <BR>
   <I>ScaleFactor:</I> ScaleFactor of Parameter <BR>
   <I>IntValue:</I> "internal" Value to be rescaled <BR>
   */
   private double ScaledVal2Val(double MinVal, double ScaleFactor,
   double IntValue) {
           double lv;
           lv = (IntValue - ScaleMinVal);
           return (MinVal + lv * ScaleFactor);
   }

}
