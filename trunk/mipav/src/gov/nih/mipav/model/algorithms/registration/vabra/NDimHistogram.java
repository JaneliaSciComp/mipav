package gov.nih.mipav.model.algorithms.registration.vabra;

import gov.nih.mipav.model.structures.ModelImage;

import java.util.HashMap;
import java.util.Iterator;
import java.util.Map.Entry;

//N-Dimensional Histogram - Sparse Hashmap Implementation
//Joint Histogram between multiple subjects and targets. By convention subjects are placed first
//Only Positive Values are alowed with in the bins
//Author - Min Chen
public class NDimHistogram extends IntensityHistogram{
	private HashMap<Integer,HistBin> bins;
	private int numOfSub;
	private int numOfTar;
	private int numOfBins;
	private int totalVol;
		
	public NDimHistogram(int numOfSub,int numOfTar,  int numOfBins) {		
		super();
		this.numOfSub = numOfSub;
		this.numOfTar = numOfTar;
		this.numOfBins = numOfBins;
		int initSize = (numOfSub + numOfTar -1)*numOfBins*numOfBins;
		bins = new HashMap<Integer,HistBin>(initSize);
		totalVol=0;
		//System.out.format("Currently Using Sparse Harsh Histograms\n");
	}
		
	//Encodes Indexes into a unique 1D hash key
	public int convertIndexesTo1D(int[] subIndexes, int[] tarIndexes){
		int index1D = 0;
		int encodeCounter = 0;
		for(int i = 0; i < subIndexes.length; i++){
			index1D += subIndexes[i]*(int)Math.pow(numOfBins,encodeCounter);
			encodeCounter++;
		}
		
		for(int i = 0; i < tarIndexes.length; i++){
			index1D += tarIndexes[i]*(int)Math.pow(numOfBins,encodeCounter);
			encodeCounter++;
		}
		
		return index1D;
	}
	
	//get bin
	public int get(int[] subIndexes, int[] tarIndexes){
		return bins.get(convertIndexesTo1D(subIndexes, tarIndexes)).value; 
	}
	
	public int get(int index1D){
		HistBin stored = bins.get(index1D);
		if(stored != null) return bins.get(index1D).value;
		else return 0;
	}

	//set bin
	public void set(int[] subIndexes, int[] tarIndexes, int val){
		int index1D = convertIndexesTo1D(subIndexes, tarIndexes);
		set(index1D, val);
	}
	public void set(int index1D, int val){
		HistBin stored = bins.get(index1D);
		if(stored != null){
			totalVol -= stored.value;
			if(val > 0){
				stored.value = val;
				totalVol += val;
			} else{
				//System.out.print("Value being set is zero or less "+ val +" \n");
				bins.remove(index1D);
			}

		}else if(val > 0){		
			bins.put(index1D, new HistBin(val));
			totalVol += val;
		}
		
		recentlyChanged = true;
	}
	
	//raise bin by one
	public void increment(int[] subIndexes, int[] tarIndexes){
		int index1D = convertIndexesTo1D(subIndexes, tarIndexes);
		HistBin stored = bins.get(index1D);
		if(stored != null) stored.value++;
		else{		
			bins.put(index1D, new HistBin(1));
		}
		totalVol++;
		recentlyChanged = true;
	}
	
	//drop bin by one	
	public void decrement(int[] subIndexes, int[] tarIndexes){
		int index1D = convertIndexesTo1D(subIndexes, tarIndexes);
		HistBin stored = bins.get(index1D);
		if(stored != null){ 
			stored.value--;
			totalVol--;
			if(stored.value <=0 ){
				bins.remove(index1D);
			}
			recentlyChanged = true;
		}		
	}	
	
	//given subject and target images, fill up the joint histogram
	public void fillJointHistogram(ModelImage[] subjects, ModelImage[] targets, int[] boundingBox){
		if((subjects.length != numOfSub) || (targets.length != numOfTar)){
			System.out.format("Subjects/Target Length Does Not Match Histogram Size\n");
			return;
		}
		
		int[] subIndexes = new int[subjects.length];
		int[] tarIndexes = new int[targets.length];
		int bin; 
		clearHist();
		
		for (int i = boundingBox[0]; i <= boundingBox[1]; i++) 
			for (int j = boundingBox[2]; j <= boundingBox[3]; j++) 
				for (int k = boundingBox[4]; k <= boundingBox[5]; k++) {

					//get bin index for all subjects
					for(int ch = 0; ch < subjects.length; ch++ ){
						bin = subjects[ch].getUByte(i, j, k);
						//bin = (bin<0?0:bin>255?255:bin);
						
						//Make sure is in bounds
						if (bin >= numOfBins){
							bin = numOfBins - 1;
							System.out.format("Value outside of bin range\n");
						}
						if (bin < 0) {
							bin = 0;
							System.out.format("Value outside of bin range\n");
						}
						subIndexes[ch] = bin;
					}
					
					//get bin index for all targets
					for(int ch = 0; ch < targets.length; ch++ ){
						bin = targets[ch].getUByte(i, j, k);
						//bin = (bin<0?0:bin>255?255:bin);
						
						//Make sure is in bounds
						if (bin >= numOfBins){
							bin = numOfBins - 1;
							System.out.format("Value outside of bin range\n");
						}
						if (bin < 0) {
							bin = 0;
							System.out.format("Value outside of bin range\n");
						}
						tarIndexes[ch] = bin;
					}
					
					//increment bin
					increment(subIndexes,tarIndexes);
		}		
		recentlyChanged = true;
	}
	
	protected void evalEntropy(){
		entropy = 0;
		double tmp;
		Iterator<HistBin> itr = bins.values().iterator();
		while(itr.hasNext()){
			tmp = ((double) itr.next().value) / totalVol;
			if(tmp <= 0){System.out.print("Invalid Bin: Value Equals Zero or less " + tmp + " \n");}
			entropy -= tmp * Math.log(tmp);//*note - Entropy is negative, hence the subtraction
		}
		recentlyChanged = false;
	}
	
	public NDimHistogram clone(){
		NDimHistogram clone = new NDimHistogram(numOfSub, numOfTar, numOfBins);
		clone.copy(this);
		return clone;
	}

	public void copy(NDimHistogram copyFrom){
		if((this.numOfSub != copyFrom.numOfSub) || (this.numOfTar != copyFrom.numOfTar) || (this.numOfBins != copyFrom.numOfBins)){
			System.out.format("Invalid Copy - Objects Don't Match\n");
			return;
		}
		
		clearHist();
		Iterator<Entry<Integer,HistBin>> itr = copyFrom.bins.entrySet().iterator();
		while(itr.hasNext()){
			Entry<Integer,HistBin> entryToCopy = itr.next();
			this.bins.put(entryToCopy.getKey().intValue(), new HistBin(entryToCopy.getValue().value));
		}
		totalVol = copyFrom.totalVol;
		recentlyChanged = true;
		//System.out.format("HIIIIIII!"+this.data.values().size() +"\n");
	}
	
	public int getNumOfSub() {return numOfSub;}
	public int getNumOfTar() {return numOfTar;}
	public int getNumOfBins() {return numOfBins;}
	public int getTotalVol() {return totalVol;}
	
	public void clearHist(){
		totalVol = 0;
		bins.clear();
		recentlyChanged = true;
	}

	
}
