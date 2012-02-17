package gov.nih.mipav.model.algorithms.t2mapping.cj.math.simplex;

import java.util.Vector;
import java.io.*;
import java.util.StringTokenizer;       // String Tokenizer
import java.lang.Integer;               // convert String to Integer
import java.lang.Float;                 // convert String to Float

/*
Declaration of ONE parameter, necessary for TParameters
*/

class TOneParam 
{
	private int cParameters;
	private double ParamValue[];
	private double ParamFitness;

	static public int TAG_UNDEFINED = 0;
	static public int TAG_V = 1;
	static public int TAG_W = 2;
	static public int TAG_R = 3;
	static public int TAG_1 = 4;
	static public int TAG_2 = 5;
	static public int TAG_e = 6;
	static public int TAG_I = 7;
	static public int TAG_E = 8;
	private int Tag = TAG_UNDEFINED;

	TOneParam (int acParameters) 
	{
		cParameters = acParameters;
		ParamValue = new double[cParameters + 1];
		for (int i = 1; (i <= cParameters); i++) {
			setValue(i,0);
		}
		ParamFitness = 0;
	}

	final public void setValue (int c, double value) {
		ParamValue[c] = value;
	}

	final public double getValue (int c) {
		return ParamValue[c];
	}

	final public void setFitness (double value) {
		ParamFitness = value;
	}

	final public double getFitness () {
		return ParamFitness;
	}

	final public void setTag(int TagText) {
		Tag = TagText;
	}

	final public int getTag() {
		return Tag;
	}

}




/**
*********************************************************************
*********************************************************************
<CENTER>
<B><BIG>SIMPLEX DataModel and History </BIG></B>
</CENTER>
<BR>

                                                            15.04.98
*********************************************************************
*********************************************************************

@author Alexander Schatten
@version 0.25
@since 30.03.98


This is the data storage class, to store the history of the Simplex
optimization process.
*/
public class TParameters {

   // VARIABLE DECLARATION

   /** Parameter Names*/
   private String ParameterName[];
   /** Fitness Name */
   private String FitnessName = "Quality";
   /** Minimum values of Parameters */
   private double ParamMinimum[];
   /** Maximum values of Parameters */
   private double ParamMaximum[];
   /** Minimum values of Fitness */
   private double FitnessMinimum = 0;
   /** Maximum values of Fitness */
   private double FitnessMaximum = 100;
   /** Value and Fitness of One Parameter */
   private TOneParam OneParam;
   /** History of Parameters and Fitness */
   protected Vector Parameters = new Vector(100,10);
   /** Filename of project file, containing all optimization parameters and history of parameters and fitness. */
   private String ParamFileName;
   /** Number of Optimization parameters */
   private int cParameters = 0;
   /** Number of values evaluated */
   protected int cValues = 0;
   /** Manual Initialization of inertial Vertex */
   private boolean ManualIni = false;
   /** Maximize or Minimize ? */
   private boolean Minimize = false;



   // CONSTRUCTORS

   /**
   Initialize new Project.
   @param
   <I>acParams:</I> Number of Parameters for new Project<BR>
   */
   public TParameters (int acParams){
      cParameters = acParams;
      ParameterName = new String[cParameters + 1];
      for (int i=1; (i <= cParameters); i++) {
         ParameterName[i] = "Param" + i;
      }
      ParamMinimum = new double[cParameters + 1];
      ParamMaximum = new double[cParameters + 1];
   }

   /**
   Initialize, but load old project file with name aParamFileName
   @param
   <I>aParamFileName:</I> Filename of <B>existing</B> Project.<BR>
   */
   public TParameters (String aParamFileName) {
      Load(aParamFileName);
   }



   // METHODS

   /**
   ToString Method, use for Printing Status of Class
   */
   public String toString() {
      String S="";
                StringBuffer SB = new StringBuffer(S);

      SB.append(
      "\n\n++++++++++++++++++++++++++++++++++++"  + "\n" +
      "\t TParamters Class:\n" +
      "++++++++++++++++++++++++++++++++++++"  + "\n" +
          "Do Minimize function = " + Minimize + "\n" +
      "Initialize manually  = " + ManualIni + "\n" +
      "cParameters          = " + cParameters + "\n" +
      "cValues              = " + cValues + "\n" +
      "ParamFileName        = " + ParamFileName + "\n" +
      "Fitness Name         = " + FitnessName + "\n" +
      "FitnessMinimum       = " + FitnessMinimum + "\n" +
      "FitnessMaximum       = " + FitnessMaximum + "\n" +
      "------------------------------------"  + "\n" +
      "Parameter Names and Range: \n" +
      "------------------------------------"  + "\n");
      for (int i = 1; (i <= cParameters); i++) {
         SB.append(getParamName (i) + "\t" +
         getMinimum (i) + " to " +
         getMaximum (i) + "\n");
      }
      SB.append(
      "------------------------------------"  + "\n" +
      "Parameter Values: \n" +
      "------------------------------------"  + "\n");
      for (int i = 1; (i<=cValues); i++) {
                   SB.append("" + i + "\t");
                   for (int j = 1; (j<=cParameters); j++) {
                      SB.append(getParSetValue (i, j) + "\t");
                   }
         SB.append("\n");
      }
      SB.append(
      "\n------------------------------------"  + "\n" +
      "Fitness Values: \n" +
      "------------------------------------"  + "\n");
      for (int i = 1; (i<=cValues); i++) {
                   SB.append("" + i + "\t" + getFitness(i) + "\n");
      }
      SB.append(
      "\n------------------------------------"  + "\n" +
      "Tags: \n" +
      "------------------------------------"  + "\n");
      for (int i = 1; (i<=cValues); i++) {
         SB.append("" + i + "\t" + getTag(i) + "\n");
      }

      SB.append(
      "------------------------------------"  + "\n");

                S = SB.toString();
      return S;

   }

   /**
   Set Minimize Property
   */
   public void setOptionMinimize (boolean DoMinimize) {
      Minimize = DoMinimize;
   }

     /**
   Get Minimize Property
   */
   public boolean getOptionMinimize () {
      return Minimize;
   }

   /**
   Set Initialization Property
   @param
   aManual if true, initialization of vertices has to be done manually;
   otherwise initial vertices are set by random generator;
   */
   public void setManualInitialization (boolean aManual) {
      ManualIni = aManual;
   }

   /**
   Get manual initialization property. If result is true,
   initialization has to be performed manually.
   */
   public boolean getManualInitialization () {
      return ManualIni;
   }


   /**
   Set Name of Parameter
   @param
   <I>ParamIdx</I> : Number of Parameter <BR>
   <I>aName</I> : Name of Parameter
   */
   public void setParamName (int ParamIdx, String aName) {
      if ( (ParamIdx > 0) & (ParamIdx <= cParameters) ) {
         ParameterName[ParamIdx] = aName;
      }
   }

   /**
   Get Parameter Name
   @param
   <I>ParamIdx</I> : Number of Parameter
   */
   public String getParamName (int ParamIdx) {
      if ( (ParamIdx > 0) & (ParamIdx <= cParameters) ) {
         return ParameterName[ParamIdx];
      } else {
         return "";
      }
   }


   /**
   Set minimal value of Parameter
   @param
   <I>ParamIdx</I> : Number of Parameter <BR>
   <I>Value</I> : minimal Value
   */
   public void setMinimum (int ParamIdx, double Value) {
      if ( (ParamIdx > 0) & (ParamIdx <= cParameters) ) {
         ParamMinimum[ParamIdx] = Value;
      }
   }

   /**
   Get minimal value of Parameter
   @param
   <I>ParamIdx</I> : Number of Parameter
   */
   public double getMinimum (int ParamIdx) {
      if ( (ParamIdx > 0) & (ParamIdx <= cParameters) ) {
         return ParamMinimum[ParamIdx];
      } else {
         return 0;
      }
   }


   /**
   Set maximal value of Parameter
   @param
   <I>ParamIdx</I> : Number of Parameter <BR>
   <I>Value</I> : maximal Value
   */
   public void setMaximum (int ParamIdx, double Value) {
      if ( (ParamIdx > 0) & (ParamIdx <= cParameters) ) {
         ParamMaximum[ParamIdx] = Value;
      }
   }

   /**
   Get maximal value of Parameter
   @param
   <I>ParamIdx</I> : Number of Parameter
   */
   public double getMaximum (int ParamIdx) {
      if ( (ParamIdx > 0) & (ParamIdx <= cParameters) ) {
         return ParamMaximum[ParamIdx];
      } else {
         return 0;
      }
   }


   /**
   Set maximal value of Fitness
   @param
   <I>Value</I> : maximal Value
   */
   public void setFitnessMaximum (double Value) {
      FitnessMaximum = Value;
   }

   /**
   Get maximal value of Fitness
   */
   public double getFitnessMaximum () {
      return FitnessMaximum;
   }


   /**
   Set minimal value of Fitness
   @param
   <I>Value</I> : maximal Value
   */
   public void setFitnessMinimum (double Value) {
      FitnessMinimum = Value;
   }

   /**
   Get minimal value of Fitness
   */
   public double getFitnessMinimum () {
      return FitnessMinimum;
   }


   /**
   Set Fitness Name
   @param
   <I>aName</I> : Name of Fitness (Quality) Parameter
   */
   public void setFitnessName (String aName) {
      FitnessName = aName;
   }

   /**
   Get Name of Fitness (Quality) Parameter
   */
   public String getFitnessName () {
      return FitnessName;
   }


   /**
   Get number of Parameters defined
   */
   public int getcParameters() {
      return cParameters;
   }


        /**
        Get number of values already added
        */
        public int getcValues(){
           return cValues;
        }


   /**
   Save current history to Project File.
   @param
   <I>aParamFile:</I> Name of Project File
   */
   public void Save (String aParamFileName) {
      ParamFileName = aParamFileName;
	 BufferedWriter W;

      try {
         W = new BufferedWriter(new FileWriter(ParamFileName));
         W.write("% Simplex Optimization Datafile\n");
         W.write("% Simplex Application Programmed by Alexander Schatten\n");
         W.write("\n");

         // Minimize/Maximize Option
         W.write("% Minimize Quality\n");
         W.write("" + Minimize + "\n");

         // Initialization manually? Option
         W.write("% Initialize Startup Simplex manually\n");
         W.write("" + ManualIni + "\n");

         // Number of Parameters
         W.write("% Number of Parameters\n");
         W.write((cParameters + 1) + "\n");

         // Number of Values
         W.write("% Number of measured Values\n");
         W.write(cValues + "\n");

         // Parameter Definition
         W.write("% Parameter Definition" + "\n");
         for (int i = 1; (i <= cParameters); i++) {
            W.write(ParameterName[i] + "\n");
            W.write(ParamMinimum[i] + "\n");
            W.write(ParamMaximum[i] + "\n");
            W.write("1" + "\n");             // Resolution not implemented yet
         }

         // Fitness Definitions
         W.write(FitnessName + "\n");
         W.write(FitnessMinimum + "\n");
         W.write(FitnessMaximum + "\n");
         W.write ("1" + "\n");                // Resolution not implemented yet

         // Data
         W.write("%Data" + "\n");
         for (int i = 1; (i <= cValues); i++) {
            for (int j = 1; (j <= cParameters); j++) {
                         W.write(getParSetValue(i, j) + "\t");
            }
            W.write(getFitness(i) + "\t");
            W.write(getTag(i) + "\n");
         }




         W.close();
      }
      catch (IOException e) {
//                        if (SimConst.DEBUG)
 //           System.out.println("Fehler beim Lesen der Datei");
      }

   }



   /**
   Load history File and continue with optimization (only for internal use).
   @param
   <I>aParamFile:</I> Name of Project File
   */
   private void Load (String aParamFileName) {
      ParamFileName = aParamFileName;
      BufferedReader F;
      StringTokenizer st;
      String S;

      try {
         F = new BufferedReader(new FileReader(ParamFileName));
         S = "%";
         while (S.indexOf("Quality") == -1) {
            S = F.readLine();
         }

         // Read Minimize Quality Option
         S = F.readLine();
         if ( S.trim().equalsIgnoreCase("true") ) {
            Minimize = true;
         } else {
            Minimize = false;
         }

         // Read Initialization Option
         S = F.readLine();
         S = F.readLine();
         if ( S.trim().equalsIgnoreCase("true") ) {
            ManualIni = true;
         } else {
            ManualIni = false;
         }

         // Read Number of Parameters
         S = F.readLine();
         S = F.readLine();
         cParameters = Integer.parseInt(S);
         cParameters--;                                  // last "param" is fitness
         ParameterName = new String[cParameters + 1];
         ParamMinimum = new double[cParameters + 1];
         ParamMaximum = new double[cParameters + 1];

         // Read Number of Values
         S = F.readLine();
         S = F.readLine();
         // cValues not allowed, because this is set by addOneParamSet() method
         int cReadValues = Integer.parseInt(S);

         // Read Parameter Names, Maximum, Minimum Values, Resolution
         S = F.readLine();
         for (int i = 1; (i <= cParameters); i++) {
            S = F.readLine();
            ParameterName[i] = S;
            S = F.readLine();
            ParamMinimum[i] = Float.valueOf(S).floatValue();
            S = F.readLine();
            ParamMaximum[i] = Float.valueOf(S).floatValue();
            S = F.readLine();
            // ignore Resolution at the moment
         }

         // Read Fitness Parameters
         S = F.readLine();
         FitnessName = S;
         S = F.readLine();
         FitnessMinimum = Float.valueOf(S).floatValue();
         S = F.readLine();
         FitnessMaximum = Float.valueOf(S).floatValue();
         S = F.readLine();
         // ignore Resolution at the moment



         // Read Data
         S = F.readLine();
         for (int i = 1; (i <= cReadValues); i++) {
            addOneParamSet();
            S = F.readLine();
                    st = new StringTokenizer(S);
            for (int j = 1; (j <= cParameters); j++) {
               S = st.nextToken();
               double fl = Float.valueOf(S).floatValue();
               setLastParSetValue (j, fl);
            }
            S = st.nextToken();
            double fl = Float.valueOf(S).floatValue();
            setLastFitness(fl);
         }

         F.close();
      }
      catch (IOException e) {
//                        if (SimConst.DEBUG)
 //           System.out.println("Fehler beim Lesen der Datei");
      }

   }



   /**
   Add one set of Parameters. All values are set to 0. This method is to
   initialize a new parameter set.
   <UL>
   <LI> Set the Parameter values by using the method setLastParSetValue.
   <LI> Set the "Fitness" value by using the method setLastParamFitness.
   </UL>
   */
   public void addOneParamSet () {
      cValues++;
      OneParam = new TOneParam   (cParameters);
      Parameters.addElement(OneParam);
   }

   /**
   Set Value for last parameterset in history list.
   @param
   <I>ParamIdx:</I> Index of Parameter in Parameterset.<BR>
   <I>Value:</I> Value of Parameter.
   */
   public boolean setLastParSetValue (int ParamIdx, double Value) {
                boolean WriteOK = false;
      if (ParamIdx <= cParameters) {
                   if ( (Value >= ParamMinimum[ParamIdx]) &
                        (Value <= ParamMaximum[ParamIdx]) ) {
                      OneParam = (TOneParam) Parameters.lastElement();
                      OneParam.setValue(ParamIdx, Value);
                      WriteOK = true;
                   }
      }
                return WriteOK;
   }

   /**
   Set Fitness for last parameterset in history list.
   @param
   <I>Value:</I> Value of Fitness.
   */
   public boolean setLastFitness (double Value) {
      if ((Value >= FitnessMinimum) & (Value <= FitnessMaximum) ) {
                   OneParam = (TOneParam) Parameters.lastElement();
                   OneParam.setFitness(Value);
                   return true;
                } else
                   return false;
   }

        /**
        Set Tag text.
        @param
        <I>ParSetNr:</I> Number of Parameterset (1-last)
        */
        public boolean setTag(int ParSetNr, int TagText) {
           if (ParSetNr <= cValues) {
              OneParam = (TOneParam) Parameters.elementAt(ParSetNr - 1);
              OneParam.setTag(TagText);
              return true;
           } else
              return false;
        }


        /**
        Set Tag text of Last Parameterset.
        */
        public void setLastTag(int TagText) {
           OneParam = (TOneParam) Parameters.lastElement();
           OneParam.setTag(TagText);
        }



   /**
   Get Value of Parameter with Index <I>ParamIdx</I> from last Parameterset in history.
   @param
   <I>ParamIdx:</I> Index of Parameter in Parameterset
   */
   public double getLastParSetValue (int ParamIdx) {
      if (ParamIdx <= cParameters) {
         OneParam = (TOneParam) Parameters.lastElement();
         return OneParam.getValue(ParamIdx);
      } else {
         return 0;
      }
   }

   /**
   Get Value of Fitness from last Parameterset in history.
   */
   public double getLastFitness() {
      OneParam = (TOneParam) Parameters.lastElement();
      return OneParam.getFitness();
   }

   /**
   Get Value of Parameter with Index <I>ParamIdx</I> of
   Parameterset <I>ParSetNr</I>.
   Do not use this method to retrieve the <I>last</I> value, use the
   getLastParSetValue method instead.
   @param
   <I>ParamIdx:</I> Index of Parameter in Parameterset <BR>
   <I>ParSetNr:</I> Index of Parameterset in History (first = 1)
   */
   public double getParSetValue (int ParSetNr, int ParamIdx) {
      if ((ParamIdx <= cParameters) & (ParSetNr <= cValues) ) {
         OneParam = (TOneParam) Parameters.elementAt(ParSetNr - 1);
         // vector starts counting at 0
         return OneParam.getValue(ParamIdx);
      } else {
         return 0;
      }
   }

   /**
   Get Value of Fitness of Parameterset <I>ParSetNr</I>.
   */
   public double getFitness(int ParSetNr) {
      if (ParSetNr <= cValues) {
               OneParam = (TOneParam) Parameters.elementAt(ParSetNr - 1);
         // vector starts counting at 0
               return OneParam.getFitness();
      } else {
         return 0;
      }
   }


        /**
        Get Tag Text
   @param
   <I>ParSetNr:</I> Index of Parameterset in History (first = 1)
        */
        public int getTag(int ParSetNr) {
           if (ParSetNr <= cValues) {
              OneParam = (TOneParam) Parameters.elementAt(ParSetNr - 1);
              return OneParam.getTag();
           } else
              return TOneParam.TAG_UNDEFINED;
        }


        /**
        Get Tag Text of last Parameter Set.
        */
        public int getLastTag() {
           OneParam = (TOneParam) Parameters.lastElement();
           return OneParam.getTag();
        }





   /**
   Remove last Parameterset from history list.
   */
   public boolean removeLastParSet () {
      if (cValues > 0) {
         Parameters.removeElementAt(cValues - 1);
         cValues--;
         return true;
      } else {
         return false;
      }
   }



}
