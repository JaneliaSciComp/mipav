package gov.nih.mipav.view.renderer.WildMagic.ProstateFramework;

import gov.nih.mipav.view.renderer.WildMagic.ProstateFramework.libsvm.svm_node;

import java.util.ArrayList;
import java.util.List;


public class FeaturesSVM {
	 
	 private List<Double> classify = new ArrayList<Double>();
     private List<svm_node[]> feature = new ArrayList<svm_node[]>();
     
     public void classAdd(int i) {
    	 classify.add((double)i);
     }
     
     public void featureAdd(svm_node[] x) { 
    	 feature.add(x);
     }
     
     public List<Double> getClassArray() {
    	 return classify;
     }
     
     public List<svm_node[]> getFeatureArray() {
    	 return feature;
     }
     
     public void disposeLocal() {
    	 classify.clear();
    	 feature.clear();
    	 classify = null;
    	 feature = null;
     }
     
}