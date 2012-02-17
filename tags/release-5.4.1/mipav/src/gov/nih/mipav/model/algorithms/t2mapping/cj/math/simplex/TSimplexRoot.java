package gov.nih.mipav.model.algorithms.t2mapping.cj.math.simplex;

/**
*********************************************************************
*********************************************************************
<CENTER>
<B><BIG>SIMPLEX Root Class - ABSTRACT PUBLIC</BIG></B>
</CENTER>
<BR>
*********************************************************************
*********************************************************************

@author Alexander Schatten
@version 0.25
@since 12.04.98

This class is the abstract basis class of the SIMPLEX optimization
system. It has to be extended by the specific optimization problem
class.

TAGS are:
<B>I</B> Initialization Vertex (random)<BR>
<B>V</B> regular Vertex<BR>
<B>R</B> first Expansion (R)<BR>
<B>E</B> second Expansion (E)<BR>
<B>1</B> CR contraction "outside" of Simplex<BR>
<B>2</B> CW, contraction "inside" of Simplex<BR>
*/



import java.util.Random;


abstract public class TSimplexRoot extends TScaledParameters {





   // VARIABLE DECLARATIONS

   /** incremental value, all <I>ProgressInterval</I> steps the
   <I>watch progress</I> */
   private int ProgressInterval = 1;
   /** max. Iteration Steps */
   private int maxIterationSteps = 100;
   /** current Iteration Step */
   private int cIteration;
   /** terminate run? if getProgress returns false, terminate run. */
   boolean DoProgress=true;
   /** random values */
   private Random RandIni = new Random();
   /** number of vertices of Simplex (cparams + 1) */
   private int cVertices;

   private double W[]  = new double[getcParameters() + 1];
   private int WorstIndex=0;
   private double WorstFitness;
   private double N[]  = new double[getcParameters() + 1];
   private int NextIndex=0;
   private double NextFitness;
   private double B[]  = new double[getcParameters() + 1];
   private int BestIndex=0;
   private double BestFitness;
   private double P[]  = new double[getcParameters() + 1];

   private double R[]  = new double[getcParameters() + 1];
   private double E[]  = new double[getcParameters() + 1];
   private double Cw[] = new double[getcParameters() + 1];
   private double Cr[] = new double[getcParameters() + 1];

   /** if true, epsilon termination is enabled in Run method */
   boolean TerminateOnEpsilon = false;
   /** "Internal" Epsilon = |Best - NextBest| */
   private double intEpsilon;
   /** "External" Epsilon: termination criterion. Only used if
   TerminateOnEpsilon is set to true with appropriate Run method*/
   private double extEpsilon;






   // CONSTRUCTORS

   /**
   Initialize new Project with default internal scaling settings:
   ScaleMin = 0; ScaleMax = 100.
   <B>do not change! otherwise random initialization hangs!</B>
   @param
   <I>acParams:</I> Number of Parameters for new Project<BR>
   */
   public TSimplexRoot(int acParams) {
      super (acParams);
   }


   /**
   Initialize and load old project file with default internal
   scaling settings: ScaleMin = 0; ScaleMax = 100.
   @param
   <I>aParamFileName:</I> Filename of <B>existing</B> project.
   */
   public TSimplexRoot(String aParamFileName) {
      super (aParamFileName);
   }







   // ABSTRACT METHODS

   /**
   Abstract Method: evaluates the current ParamValues; only abstract
   declarated here, must be overwritten in the concrete implementation
   @param
   <I>cParams:</I> Number of Parameters <BR>
   <I>ParamValue[]: </I> array of Parameter Values from
   ParamValue[1] to ParamValue[cParams]
   */
   abstract public double evalParams(int cParams, double ParamValue[]);


   /**
   Abstract Method: is called all <I>ProgressIntervall</I> calculation steps.
   Overwrite this method with any procedure that controls a progress bar or
   the like.
   If <I>return value</I> is false, the calculation is terminated.
   */
   abstract public boolean watchProgress(int StepNr);


   /**
   Abstract method: Overwrite to create a report depending on the optimization
   problem.
   @param
   <I>cParams:</I> number of parameters in optimization<BR>
   <I>ParamValue[]: </I> array of Parameter Values from
   ParamValue[1] to ParamValue[cParams] <BR>
   <I>Terminated:</I> if Terminated is true, calculation was terminated by
   watchProgress() method, otherwise number of iteration steps was reached.
   */
   abstract public void makeReport(int cParams, String ParamName[],
   double ParamValue[], boolean Terminated);






   // PUBLIC METHODS

   /**
   set interval value. All <I>aInterval</I> calculation steps, the
   watchProgress() method is called
   @param
   <I>aInterval: </I> Interval.
   */
   public void setProgressInterval(int aInterval){
      ProgressInterval = aInterval;
   }


   /** get ProgressInterval value */
   public int getProgressInterval() {
      return ProgressInterval;
   }


   /**
   RUN, start Simplex optimization. Does the same as Run(int), but adds an
   termination criterion with the epsilon value.
   @param
   <I>aMaxIterationSteps:</I> number of maximal interation steps
   <I>aEpsilon</I>: Parameter for termination criterion; if Fitness of
   |BestVertex - NextBestVertex| <= aEpsilon, the optimization is terminated,
   even if the aMaxIterationSteps are not fulfilled.
   */
   public void Run(double aEpsilon, int aMaxIterationSteps) {
      extEpsilon = aEpsilon;
      TerminateOnEpsilon = true;
      if (aMaxIterationSteps > 0)
          maxIterationSteps = aMaxIterationSteps;
      Run();
   }


   /**
   RUN, start Simplex optimization
   @param
   <I>aMaxIterationSteps:</I> number of maximal interation steps
   */
   public void Run(int aMaxIterationSteps) {
      if (aMaxIterationSteps > 0)
          maxIterationSteps = aMaxIterationSteps;
      TerminateOnEpsilon = false;
      Run();
   }


   /**
   RUN, start Simplex optimization; private method. use Run(double, int) or
   Run (int). This will initialize the Run parameters and call this method.
   */
   private void Run() 
   {
      boolean MayBreak = false;

	  // CJ: Added the three lines so that Run could be 
	  // CJ: called several times in a row.

	  if(  true )
	  {
		  cValues = 0;
		  Parameters.clear();
	  }
	  else
	  {
		  if( cValues > 4 )
		  {
			  cValues = 4;
			  while( Parameters.size() > cValues + 1 )
			  {
				  Parameters.remove(4);
			  }
		   }
	  }
//	  intEpsilon = Double.POSITIVE_INFINITY;

      cVertices = (getcParameters() + 1);

      //for (int i=1; i<=maxIterationSteps; i++){
      int i = 1;
      while (getcValues()<maxIterationSteps) {
         DoOneStep();
         if ((i++ % ProgressInterval)==0) {
            DoProgress = watchProgress(i);
         }
         if (getcValues() > cVertices + 4)   // otherwise initializ. not ready
            MayBreak = true;
         if ((!DoProgress) & (MayBreak))
            break;
         if ( (MayBreak) & (TerminateOnEpsilon) & (intEpsilon <= extEpsilon) )
            break;
      }

      // call report method with best Vertex of last Simplex

      if ((getcParameters() > 0) & (getcValues() > 0)) {

         double ParamValue[] = new double[getcParameters() + 1];
         String ParamName[] = new String[getcParameters() + 1];

         CalcWNB_PR();
         for (i=1; (i<=getcParameters()); i++) {
            ParamName[i] = getParamName(i);
            ParamValue[i] = getParSetValue(BestIndex, i);
         }
      makeReport(getcParameters(), ParamName, ParamValue, DoProgress);
      }

   }


   /**
   Do One Step; this method is called by the run procedure, and performs just
   <I>one</I> optimization step.
   */
   private void DoOneStep() {

      double ParamValue[] = new double[getcParameters() + 1];

		if (getcValues() < cVertices) 
		{
			// do random initialization

			double RandValue;

			addOneParamSet();

			for (int i=1; (i<=getcParameters()); i++) {
				RandValue = (RandIni.nextDouble() * 100f);
				setScaledLastParSetValue(i, RandValue);
			}

			setLastTag(TOneParam.TAG_I);

			// evaluate vertex
			for (int i=1; (i<=getcParameters()); i++)
				ParamValue[i] = getLastParSetValue(i);
				if (getOptionMinimize()) {
					setLastFitness(getFitnessMaximum() -
					evalParams(getcParameters(), ParamValue));
				} 
				else
				{
					setLastFitness(evalParams(getcParameters(), ParamValue));
				}
		}

      else {
         if (CalculateNextVertex()) {
            // evaluate vertex
            for (int i=1; (i<=getcParameters()); i++)
               ParamValue[i] = getLastParSetValue(i);
            if (getOptionMinimize()) {
               setLastFitness(getFitnessMaximum() -
               evalParams(getcParameters(), ParamValue));
            } else
               setLastFitness(evalParams(getcParameters(), ParamValue));
         } else {
            setScaledLastFitness(0);
         }
      }

   }








   // PRIVATE METHODS

   /**
   "Heart" of the algorithm. Calculates the next vertex on the basis of the
   vertices in the history. Expansion and Contraction of Simplex are performed
   if necessary.
   returns <I>true</I>, if claculated value in bounds of parameters,
   <I>false</I> if not (hence new vertex is invalid).
   */
   private boolean CalculateNextVertex() {

      boolean newVertexInBounds = true;
      int LastTag = TOneParam.TAG_UNDEFINED;

      LastTag = getLastTag();
      if (SimConst.DEBUG)
         System.out.println("LastTag = " + LastTag);

      if (LastTag == TOneParam.TAG_I) {
         // ********************************************************
         // last is Initial vertex
         // find worst vertex
         double Worst=100;
         int WorstIndex=0;
         for (int i=1; (i <= getcValues()); i++) {
            if (getScaledFitness(i) <= Worst) {
               Worst = getScaledFitness(i);
               WorstIndex = i;
            }
         }
         // calculate P without worst Vertex
         for (int j=1; (j <= getcParameters()); j++) {
            double Summe = 0;
            for (int i=1; (i <= getcValues()); i++) {
               if (i != WorstIndex) {
                  Summe += getParSetValue(i,j);
               }
            }
            P[j] = (Summe/(cVertices - 1));
         }
         // reset "I" tag to "V"
         for (int i=1; (i <= getcValues()); i++)
            setTag(i, TOneParam.TAG_V);
         // calculate R
         for (int i=1; (i <= getcParameters()); i++)
            R[i] = P[i] + (P[i] - getParSetValue(WorstIndex,i));
         // write R to history
         addOneParamSet();
         for (int i=1; (i <= getcParameters()); i++)
            if (! setLastParSetValue(i, R[i]))
               newVertexInBounds = false;
         if (newVertexInBounds)
            setLastTag(TOneParam.TAG_R);    // o.k. proceed
         else
            setLastTag(TOneParam.TAG_2);    // do contraction
      } else
      if (LastTag == TOneParam.TAG_R) {
         // ********************************************************
         // first expansion
         if (SimConst.DEBUG) System.out.println("calculate R");
         double RFitness = getLastFitness();
         if (RFitness > BestFitness) {
            setLastTag(TOneParam.TAG_E);
            LastTag = TOneParam.TAG_E;
         }
         else {
            if (RFitness > NextFitness) {
               setLastTag(TOneParam.TAG_V);           // o.k. accept R as new vertex
               LastTag = TOneParam.TAG_V;
               setTag(WorstIndex, TOneParam.TAG_W);
            }
            else {
               if (RFitness > WorstFitness) {
                  setLastTag(TOneParam.TAG_1);
                  LastTag = TOneParam.TAG_1;
               }
               else {
                  setLastTag(TOneParam.TAG_2);
                  LastTag = TOneParam.TAG_2;
               }
            }
         }
         // ********************************************************
      } else
      if (LastTag == TOneParam.TAG_E) {
         // ********************************************************
         // second expansion evaluation
         // ********************************************************
         if (SimConst.DEBUG) System.out.println("calculate E");
         CalcWNB_PR();
         addOneParamSet();
         newVertexInBounds = true;
         for (int i=1; (i <= getcParameters()); i++) {
            E[i] = (R[i] + (P[i] - getParSetValue(WorstIndex,i)));
            if (! setLastParSetValue(i, E[i]))
               newVertexInBounds = false;
         }
         if (newVertexInBounds)
            setLastTag(TOneParam.TAG_e);       // o.k. check E
         else
            setLastTag(TOneParam.TAG_2);       // out of bounds: do contraction!
         // ********************************************************
      } else
      if (LastTag == TOneParam.TAG_e) {
         // ********************************************************
         // second expansion set
         if (SimConst.DEBUG) System.out.println("calculate e");
         double EFitness = getLastFitness();
         CalcWNB_PR();
         if (EFitness > BestFitness) {
            setLastTag(TOneParam.TAG_V);                   // o.k. Take the E vertex
            setTag(WorstIndex, TOneParam.TAG_W);
            LastTag=TOneParam.TAG_V;
         }
         else {
            removeLastParSet();     // nicht optimal evt. korrigieren...
            setLastTag(TOneParam.TAG_V);
            setTag(WorstIndex, TOneParam.TAG_W);
            LastTag=TOneParam.TAG_V;
         }
         // ********************************************************
      } else
      if (LastTag == TOneParam.TAG_V) {
         // ********************************************************
         if (SimConst.DEBUG) System.out.println("calculate V");
         // regular vertex
         CalcWNB_PR();
         // write R to history
         newVertexInBounds = true;
         addOneParamSet();
         for (int i=1; (i <= getcParameters()); i++)
            if (! setLastParSetValue(i, R[i]))
               newVertexInBounds = false;
         if (newVertexInBounds)
            setLastTag(TOneParam.TAG_R);    // o.k. proceed
         else
            setLastTag(TOneParam.TAG_2);    // do contraction
         // ********************************************************
      } else
      if (LastTag == TOneParam.TAG_1) {
         // ********************************************************
         // calculate CR
         CalcWNB_PR();
         if (SimConst.DEBUG) System.out.println("calculate 1 (CR)");
         for (int i=1; (i <= getcParameters()); i++)
            Cr[i] = ( P[i] + ( (P[i] - getParSetValue(WorstIndex,i)) / 2d ) );
         // write CR to history
         addOneParamSet();
         newVertexInBounds = true;
         for (int i=1; (i <= getcParameters()); i++)
            if (! setLastParSetValue(i, Cr[i]))
               newVertexInBounds = false;
         if (newVertexInBounds) {
            setLastTag(TOneParam.TAG_V);       // result regular in this case
            setTag(WorstIndex, TOneParam.TAG_W);
         }
         else
            setLastTag(TOneParam.TAG_2);       // out of bounds: do contraction!

         // ********************************************************
      } else
      if (LastTag == TOneParam.TAG_2) {
         // ********************************************************
         // calculate CW
         if (SimConst.DEBUG) System.out.println("calculate 2 (CW)");
         CalcWNB_PR();
         for (int i=1; (i <= getcParameters()); i++)
            Cw[i] = ( P[i] - ( (P[i] - getParSetValue(WorstIndex,i)) / 2d ) );
         // write Cw to history
         addOneParamSet();
         for (int i=1; (i <= getcParameters()); i++)
            setLastParSetValue(i, Cw[i]);
         setLastTag(TOneParam.TAG_V);
         setTag(WorstIndex, TOneParam.TAG_W);
         // ********************************************************
      };

      return newVertexInBounds;
   }


   /**
   private method, only for internal use:
   <UL>
   <LI> find Worst, Best and Nextbest vertices of current simplex
   <LI> calculate center of gravity <I>P</I>
   <LI> calculate expansion point <I>R</I>
   </UL>
   */
   private void CalcWNB_PR() {

      // find worst vertex
      double Worst=100;
      int i = getcValues();
      int cV = 0;
      while (cV < cVertices){
         if (getTag(i) == TOneParam.TAG_V) {
            cV++;
            if (getScaledFitness(i) <= Worst) {
               Worst = getScaledFitness(i);
               WorstIndex = i;
            }
            /* if (SimConst.DEBUG)
               System.out.println("regular Vertec at index " + i); */
         }
         i--;
      }

      // find best vertex
      double Best=0;
      i = getcValues();
      cV = 0;
      while (cV < cVertices){
         if (getTag(i) == TOneParam.TAG_V) {
            cV++;
            if (getScaledFitness(i) >= Best) {
               Best = getScaledFitness(i);
               BestIndex = i;
            }
         }
         i--;
      }

      // find next best vertex
      double Next=0;
      i = getcValues();
      cV = 0;
      while (cV < cVertices){
         if (getTag(i) == TOneParam.TAG_V) {
            cV++;
            if ( (getScaledFitness(i) >= Next) &
                 (i != WorstIndex) & (i != BestIndex) ) {
               Next = getScaledFitness(i);
               NextIndex = i;
            }
         }
         i--;
      }
      /*if (SimConst.DEBUG) {
         System.out.println("\nWorst Index = " + WorstIndex);
         System.out.println("Next-Best Index = " + NextIndex);
         System.out.println("Best Index = " + BestIndex);
      } */
      WorstFitness = getFitness(WorstIndex);
      BestFitness = getFitness(BestIndex);
      NextFitness = getFitness(NextIndex);
      intEpsilon = Math.abs(BestFitness - NextFitness);

      // calculate P without worst Vertex
      for (int j=1; (j <= getcParameters()); j++) {
         double Summe = 0;
         cV = 0;
         i = getcValues();
         while (cV < cVertices) {
            if (getTag(i) == TOneParam.TAG_V) {
               cV++;
               if (i != WorstIndex)
                  Summe += getParSetValue(i,j);
            }
            i--;
         }
         // if (SimConst.DEBUG) System.out.println("Summe " + j + " = " + Summe);
         P[j] = (Summe/(cVertices - 1));
      }

      // calculate R
      for (i=1; (i <= getcParameters()); i++)
         R[i] = P[i] + (P[i] - getParSetValue(WorstIndex,i));
   }


}
