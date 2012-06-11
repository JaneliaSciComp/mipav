package gov.nih.mipav.model.algorithms.registration.vabra;

import java.util.HashMap;
import java.util.Iterator;

/*A modifier of an existing N-Dimensional Histogram
 * This modifier works off an existing joint histogram by only keeping track of bins that have changed.
 * This saves memory by avoiding unnecessary copies and speeds up entropy calculations.
 * Primary use is for optimization tests where small modifications are made to an histogram in
 * order to find some optimal matching criterion. This allows easy reset and retests.
 * 
 * IMPORTANT NOTE - In order for the quick update and in place entropy calculations to work, 
 * the total volume of the histogram must stay constant. Therefore only movement of values between bins
 *  are allowed. Use a full histogram if you need to modify specific bins.  
 * 
 * 
 * 
 * Author - Min Chen
*/
public class NDimHistogramModifier extends IntensityHistogram{
	private NDimHistogram refHist;//Reference Histogram
	private HashMap<Integer,HistBin> modifiedBins;
		
	public NDimHistogramModifier(NDimHistogram refHist) {
		super();
		this.refHist = refHist;
		int initSize = (refHist.getNumOfSub() + refHist.getNumOfTar() -1)*refHist.getNumOfBins();
		modifiedBins = new HashMap<Integer,HistBin>(initSize);
		//System.out.format("Currently Using a Modifier of Sparse Harsh Histograms\n");
	}
			
	//get bin
	public int get(int[] subIndexes, int[] tarIndexes){
		return modifiedBins.get(refHist.convertIndexesTo1D(subIndexes, tarIndexes)).value; 
	}

	//Performs a move by decrementing a bin and an incrementing another bin
	//The only way to change bin values in this modifier so that volume is always constant
	public void move(int[] subIndexesFrom, int[] tarIndexesFrom, int[] subIndexesTo, int[] tarIndexesTo){
		int index1DFrom = refHist.convertIndexesTo1D(subIndexesFrom, tarIndexesFrom);
		int index1DTo = refHist.convertIndexesTo1D(subIndexesTo, tarIndexesTo);
	
		//Decrement From bin
		HistBin storedFrom = modifiedBins.get(index1DFrom);
		if(storedFrom != null) storedFrom.value--;
		else modifiedBins.put(index1DFrom, new HistBin(-1));
		
		//Increment To bin
		HistBin storedTo = modifiedBins.get(index1DTo);
		if(storedTo != null) storedTo.value++;
		else modifiedBins.put(index1DTo, new HistBin(1));
		
		recentlyChanged = true;
	}

	protected void evalEntropy(){
		double oldValProb, newValProb;
		int key, oldVal, newVal;
		int totalVol = refHist.getTotalVol();
		 
		entropy = refHist.getEntropy();
		Iterator<Integer> itr = modifiedBins.keySet().iterator();
		while(itr.hasNext()){
			key = itr.next().intValue();
			oldVal = refHist.get(key);
			newVal = oldVal + modifiedBins.get(key).value;
		
			//remove the contributions from the old values of modified bins from reference
			//*note - Entropy is negative, hence the addition
			if(oldVal > 0){
				oldValProb = ((double) oldVal) / totalVol;
				entropy += oldValProb * Math.log(oldValProb);
			}				

			//and put in entropy contributions from the new values
			//*note - Entropy is negative, hence the subtraction
			if(newVal > 0){
				newValProb = ((double) newVal) / totalVol;
				entropy -= newValProb * Math.log(newValProb);
			}
		}
		recentlyChanged = false;
	}
	/*
	public NDimHistogramModifier clone(){
		NDimHistogramModifier clone = new NDimHistogramModifier(this.refHist);
		clone.copy(this);
		return clone;
	}

	public void copy(NDimHistogramModifier copyFrom){
		this.refHist = copyFrom.refHist;
		reset();
		Iterator<Entry<Integer,HistBin>> itr = copyFrom.modifiedBins.entrySet().iterator();
		while(itr.hasNext()){
			Entry<Integer,HistBin> entryToCopy = itr.next();
			this.modifiedBins.put(entryToCopy.getKey().intValue(), new HistBin(entryToCopy.getValue().value));
		}
		recentlyChanged = true;
		//System.out.format("HIIIIIII!"+this.data.values().size() +"\n");
	}
*/	
	
	public void commitModifierToRefHist(){
		
		int key, newVal, oldVal;
		Iterator<Integer> itr = modifiedBins.keySet().iterator();
		//set reference bins to the modified bins
		//System.out.print("Ref Entropy Before:"+refHist.getEntropy()+"\n");
		//System.out.print("Temp Entropy Before:"+this.getEntropy()+"\n");
		while(itr.hasNext()){
			key = itr.next().intValue();
			oldVal = refHist.get(key);
			newVal = oldVal + modifiedBins.get(key).value;
			
			//if(newVal <= 0)System.out.print("Value being set is zero or less"+ oldVal + " " + newVal +"\n");
			refHist.set(key, newVal);
		}
		//System.out.print("Ref Entropy After:"+refHist.getEntropy()+"\n");
		//System.out.print("Temp Entropy AFter:"+this.getEntropy()+"\n");		
		//System.out.print("Commiting!\n");
		recentlyChanged = true;
		reset();
	}
	
	public void reset(){
		modifiedBins.clear();
		recentlyChanged = true;
	}

	public int getTotalVol() {return refHist.getTotalVol();}
	
}
