package gov.nih.mipav.view.renderer.WildMagic.ProstateFramework;

import gov.nih.mipav.view.renderer.WildMagic.ProstateFramework.liblinearsvm.FeatureNode;

import java.util.ArrayList;
import java.util.List;


public class Features {
	 
	 private List<Integer> classify = new ArrayList<Integer>();
     private List<FeatureNode[]> feature = new ArrayList<FeatureNode[]>();
     
     public void classAdd(int i) {
    	 classify.add(i);
     }
     
     public void featureAdd(FeatureNode[] x) { 
    	 feature.add(x);
     }
     
     public List<Integer> getClassArray() {
    	 return classify;
     }
     
     public List<FeatureNode[]> getFeatureArray() {
    	 return feature;
     }
     
     public void disposeLocal() {
    	 classify.clear();
    	 feature.clear();
    	 classify = null;
    	 feature = null;
     }
     
}