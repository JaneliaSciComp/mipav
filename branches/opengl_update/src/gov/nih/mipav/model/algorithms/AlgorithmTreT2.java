package gov.nih.mipav.model.algorithms;

import gov.nih.mipav.model.file.FileInfoBase;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.ViewJFrameImage;
import gov.nih.mipav.view.ViewUserInterface;
import gov.nih.mipav.view.dialogs.JDialogTreT2;

import java.io.IOException;

/** 
 * This class is adapted from the ImageJ version originally written by Sean Deoni while working at the
 * National Institutes of Health. This class performs T2 calculations given any number of
 * SPGR images.
 * 
 **/

public class AlgorithmTreT2 extends AlgorithmTProcess {

    private double[] treFA_phase0;
    private double[] treFA_phase180;
    
    private int[] ssfpImageIndex_phase0;
    private int[] ssfpImageIndex_phase180;
    private int t1ImageIndex;
    private int b1ImageIndex;
    
    private double[] simplexLineValues, simplexResiduals, simplexCentre, reflection, expansion, contraction, shrink;
    private double[][] simplex;
    private double[] twoPSimplexLineValues, twoPSimplexResiduals, twoPSimplexCentre, twoPReflection, twoPExpansion, twoPContraction, twoPShrink;
    private double[][] twoPSimplex;
    private int[] bestToWorst;
    
    @SuppressWarnings("unused")
    private boolean hardInterrupt = false;
    
    private ModelImage t2ResultStack;
    private ModelImage m0ResultStack;
    private ModelImage r2ResultStack;
    private ModelImage b0ResultStack;
    
    private String[] wList;
    
    /** The frames for result images (if null at end of algorithm src ModelImage is destroyed) */
    private ViewJFrameImage t2ResultWindow;
    private ViewJFrameImage m0ResultWindow;
    private ViewJFrameImage r2ResultWindow;
    private ViewJFrameImage b0ResultWindow;
    
    /** The dialog for accessing GUI specific information, also set during scripting. **/
    private JDialogTreT2 dialog;
    
    public AlgorithmTreT2(JDialogTreT2 dialog, double[] treFAPhase0,
            double[] treFAPhase180, int[] ssfpImageIndexPhase0,
            int[] ssfpImageIndexPhase180, int t1ImageIndex, int b1ImageIndex,
            double[] simplexLineValues, double[] simplexResiduals,
            double[] simplexCentre, double[] reflection, double[] expansion,
            double[] contraction, double[] shrink, double[][] simplex,
            double[] twoPSimplexLineValues, double[] twoPSimplexResiduals,
            double[] twoPSimplexCentre, double[] twoPReflection,
            double[] twoPExpansion, double[] twoPContraction,
            double[] twoPShrink, double[][] twoPSimplex, int[] bestToWorst,
            String[] wList) {
        super();
        this.dialog = dialog;
        treFA_phase0 = treFAPhase0;
        treFA_phase180 = treFAPhase180;
        ssfpImageIndex_phase0 = ssfpImageIndexPhase0;
        ssfpImageIndex_phase180 = ssfpImageIndexPhase180;
        this.t1ImageIndex = t1ImageIndex;
        this.b1ImageIndex = b1ImageIndex;
        this.simplexLineValues = simplexLineValues;
        this.simplexResiduals = simplexResiduals;
        this.simplexCentre = simplexCentre;
        this.reflection = reflection;
        this.expansion = expansion;
        this.contraction = contraction;
        this.shrink = shrink;
        this.simplex = simplex;
        this.twoPSimplexLineValues = twoPSimplexLineValues;
        this.twoPSimplexResiduals = twoPSimplexResiduals;
        this.twoPSimplexCentre = twoPSimplexCentre;
        this.twoPReflection = twoPReflection;
        this.twoPExpansion = twoPExpansion;
        this.twoPContraction = twoPContraction;
        this.twoPShrink = twoPShrink;
        this.twoPSimplex = twoPSimplex;
        this.bestToWorst = bestToWorst;
        this.wList = wList;
    }

    public AlgorithmTreT2(ModelImage destImage, ModelImage srcImage, 
            JDialogTreT2 dialog, double[] treFAPhase0, double[] treFAPhase180,
            int[] ssfpImageIndexPhase0, int[] ssfpImageIndexPhase180,
            int t1ImageIndex, int b1ImageIndex, double[] simplexLineValues,
            double[] simplexResiduals, double[] simplexCentre,
            double[] reflection, double[] expansion, double[] contraction,
            double[] shrink, double[][] simplex,
            double[] twoPSimplexLineValues, double[] twoPSimplexResiduals,
            double[] twoPSimplexCentre, double[] twoPReflection,
            double[] twoPExpansion, double[] twoPContraction,
            double[] twoPShrink, double[][] twoPSimplex, int[] bestToWorst,
            String[] wList) {
        super(destImage, srcImage);
        this.dialog = dialog;
        treFA_phase0 = treFAPhase0;
        treFA_phase180 = treFAPhase180;
        ssfpImageIndex_phase0 = ssfpImageIndexPhase0;
        ssfpImageIndex_phase180 = ssfpImageIndexPhase180;
        this.t1ImageIndex = t1ImageIndex;
        this.b1ImageIndex = b1ImageIndex;
        this.simplexLineValues = simplexLineValues;
        this.simplexResiduals = simplexResiduals;
        this.simplexCentre = simplexCentre;
        this.reflection = reflection;
        this.expansion = expansion;
        this.contraction = contraction;
        this.shrink = shrink;
        this.simplex = simplex;
        this.twoPSimplexLineValues = twoPSimplexLineValues;
        this.twoPSimplexResiduals = twoPSimplexResiduals;
        this.twoPSimplexCentre = twoPSimplexCentre;
        this.twoPReflection = twoPReflection;
        this.twoPExpansion = twoPExpansion;
        this.twoPContraction = twoPContraction;
        this.twoPShrink = twoPShrink;
        this.twoPSimplex = twoPSimplex;
        this.bestToWorst = bestToWorst;
        this.wList = wList;
    }



    /**
     * Prepares this class for destruction.
     */
    public void finalize() {
        if(t2ResultWindow == null && t2ResultStack != null) {
            t2ResultStack.disposeLocal();
        }
        
        if(m0ResultWindow == null && m0ResultStack != null) {
            m0ResultStack.disposeLocal();
        }

        if(r2ResultWindow == null && r2ResultStack != null) {
            r2ResultStack.disposeLocal();
        }

        if(b0ResultWindow == null && b0ResultStack != null) {
            b0ResultStack.disposeLocal();
        }
    }
    
    public void runAlgorithm() {
        if (dialog.isPerformFullModelling() == true || dialog.isPerformApproxModelling() == true) {
            if (dialog.isPerformFullModelling() == true) calculateT2withFullModelling();
            else calculateT2withApproximateModelling();
        }
        else {
            if (dialog.isPerformConventionalWith0Phase() == true) calculateT2with0Phase();
            else calculateT2with180Phase();
        }
    }
    
    public void calculateT2with0Phase() {
        ModelImage image;
        
        double[] fa_phase0;
        double[] scaledFA_phase0;
        
        double[][] ssfpPixelValues_phase0;
        double[] t1PixelValues, b1PixelValues;
        double[] phase0Data;
        
        float[][] t2Values, m0Values, r2Values;
        
        double a, d, e2;
        double sumX, sumY, sumXY, sumXX, slope, denominator, intercept, t2, e1, m0, r2;
        
        int width, height, nSlices, tSeries;
        int x,y,k,t,angle, p, pixelIndex;
        
        image = ViewUserInterface.getReference().getRegisteredImageByName(wList[ssfpImageIndex_phase0[0]]);
        width = image.getExtents()[0];
        height = image.getExtents()[1];
        if(image.getNDims() > 2) {
            nSlices = image.getExtents()[2];
        } else {
            nSlices = 1;
        }
        
        boolean do4D = false;
        tSeries = 1;
        for (angle=0; angle<dialog.getNfa_phase0(); angle++) {
            image = ViewUserInterface.getReference().getRegisteredImageByName(wList[ssfpImageIndex_phase0[angle]]);
            if(image.getNDims() > 3 && !do4D) { //clause is only entered once
                do4D = true;
                tSeries = image.getExtents()[3];
                t2ResultStack = new ModelImage(ModelImage.DOUBLE, image.getExtents(), "t2_results");
                m0ResultStack = new ModelImage(ModelImage.DOUBLE, image.getExtents(), "m0_results");
                r2ResultStack = new ModelImage(ModelImage.DOUBLE, image.getExtents(), "r2_results");
                
                t2ResultStack = nearCloneImage(image, t2ResultStack);
                m0ResultStack = nearCloneImage(image, m0ResultStack);
                r2ResultStack = nearCloneImage(image, r2ResultStack);
            }
        }
        
        if(!do4D) {
            t2ResultStack = new ModelImage(ModelImage.DOUBLE, image.getExtents(), "t2_results");
            m0ResultStack = new ModelImage(ModelImage.DOUBLE, image.getExtents(), "m0_results");
            r2ResultStack = new ModelImage(ModelImage.DOUBLE, image.getExtents(), "r2_results");
            
            t2ResultStack = nearCloneImage(image, t2ResultStack);
            m0ResultStack = nearCloneImage(image, m0ResultStack);
            r2ResultStack = nearCloneImage(image, r2ResultStack);
        }
        
        String prefix = new String();
        // Actually perform the T2 Calculations
        for(t=0; t<tSeries; t++) {
            if(do4D) {
                prefix = "Series "+t+": ";
            } else {
                prefix = "";
            }
            
            ssfpPixelValues_phase0 = new double[dialog.getNfa_phase0()][width*height];
            t1PixelValues = new double[width*height];
            if (dialog.isIncludeB1Map()) { 
                b1PixelValues = new double[width*height];
            }
            else {
                b1PixelValues = new double[1];
            }
            
            if (dialog.isCalculateT2()) { 
                t2Values = new float[nSlices][width*height];
            }
            else { 
                t2Values = new float[1][1];
            }
            if (dialog.isCalculateM0()) { 
                m0Values = new float[nSlices][width*height];
            }
            else { 
                m0Values = new float[1][1];
            }
            if (dialog.isInvertT2toR2()) { 
                r2Values = new float[nSlices][width*height];
            }
            else { 
                r2Values = new float[1][1];
            }
            
            
            fa_phase0 = new double[dialog.getNfa_phase0()];
            scaledFA_phase0 = new double[dialog.getNfa_phase0()];
            for (angle=0; angle<dialog.getNfa_phase0(); angle++) {
                fa_phase0[angle] = Math.toRadians(treFA_phase0[angle]);
            }
            
            phase0Data = new double[dialog.getNfa_phase0()];
            
            for (k=0; k<nSlices; k++) {
                fireProgressStateChanged(prefix+"calculating T2 values on slice: "+k+" of "+(nSlices-1));
                fireProgressStateChanged(0+(int)((float)k/(float)nSlices*80.0));
                if(interrupted()) {
                    hardInterrupt = true;
                    return;
                }
                // grab the ssfp pixel values from the phase = 0 data
                for (angle=0; angle<dialog.getNfa_phase0(); angle++) {
                    image = ViewUserInterface.getReference().getRegisteredImageByName(wList[ssfpImageIndex_phase0[angle]]);
                    pixelIndex = 0;
                    for (y=0; y<height; y++) {
                        for (x=0; x<width; x++) {
                            if(image.getNDims() < 4) {
                                ssfpPixelValues_phase0[angle][pixelIndex] = image.getDouble(x, y, k);
                            } else {
                                ssfpPixelValues_phase0[angle][pixelIndex] = image.getDouble(x, y, k, t);
                            }
                            pixelIndex++;
                        }
                    }
                }
            
                // grab the T1 and B1 information
                image = ViewUserInterface.getReference().getRegisteredImageByName(wList[t1ImageIndex]);
                
                pixelIndex = 0;
                for (y=0; y<height; y++) {
                    for (x=0; x<width; x++) {
                        if(image.getNDims() < 4) {
                            t1PixelValues[pixelIndex] = image.getDouble(x, y, k);
                        } else {
                            t1PixelValues[pixelIndex] = image.getDouble(x, y, k, t);
                        }
                        pixelIndex++;
                    }
                }
                
                if (dialog.isIncludeB1Map() == true) {
                    image = ViewUserInterface.getReference().getRegisteredImageByName(wList[b1ImageIndex]);
                    
                    pixelIndex = 0;
                    for (y=0; y<height; y++) {
                        for (x=0; x<width; x++) {
                            if(image.getNDims() < 4) {    
                                b1PixelValues[pixelIndex] = image.getDouble(x,y, k);
                            } else {
                                b1PixelValues[pixelIndex] = image.getDouble(x,y, k, t);
                            }
                            pixelIndex++;
                        }
                    }
                }
            
                // now that we have all the information, perform the calculate pixel-wise
                pixelIndex = 0;
                for (x=0; x<width; x++) {
                    for (y=0; y<height; y++) {
                        
                        if (t1PixelValues[pixelIndex] > 0.00) {
                            
                            e1 = Math.exp(-dialog.getTreTR()/t1PixelValues[pixelIndex]);
                            
                            // scale up (or down) the flip angles based on the calculated B1 if required
                            if (dialog.isIncludeB1Map() == true) {
                                for (p=0; p<dialog.getNfa_phase0(); p++) {
                                    scaledFA_phase0[p] = fa_phase0[p]*b1PixelValues[pixelIndex];
                                }
                            }
                            else {
                                for (p=0; p<dialog.getNfa_phase0(); p++) {
                                    scaledFA_phase0[p] = fa_phase0[p];
                                }
                            }
                            
                            // grab the SSFP values for this pixel
                            for (p=0; p<dialog.getNfa_phase0(); p++) phase0Data[p] = ssfpPixelValues_phase0[p][pixelIndex];
                            
                            intercept = 1.00;
                            denominator = 1.00;
                            slope = 0.00;
                            e2 = 1.00;
                            t2 = 0.00;
                            
                            // calculate T2 first from the phase = 0 data
                            sumX = 0.00;
                            sumY = 0.00;
                            sumXY = 0.00;
                            sumXX = 0.00;
                            
                            for (p=0; p<dialog.getNfa_phase0(); p++) {
                                sumX += phase0Data[p]/Math.tan(scaledFA_phase0[p]);
                                sumY += phase0Data[p]/Math.sin(scaledFA_phase0[p]);
                                sumXY += (phase0Data[p]/Math.tan(scaledFA_phase0[p]))*(phase0Data[p]/Math.sin(scaledFA_phase0[p]));
                                sumXX += (phase0Data[p]/Math.tan(scaledFA_phase0[p]))*(phase0Data[p]/Math.tan(scaledFA_phase0[p]));
                            }
                            
                            d = (dialog.getNfa_phase0()*sumXX) - sumX*sumX;
                            a = (dialog.getNfa_phase0()*sumXY) - (sumX*sumY);
                            
                            if (d != 0) {
                                slope = a/d;
                                denominator = (e1-slope)/(1.00-slope*e1);
                                intercept = (sumY-slope*sumX)/dialog.getNfa_phase0();
                                
                                if (denominator > 0.00 && denominator < 1.00) {
                                    t2 = -dialog.getTreTR()/Math.log(denominator);
                                    e2 = Math.exp(-dialog.getTreTR()/t2);
                                    m0 = intercept*(1.00-e1*e2)/(1.00-e1);
                                }
                                else {
                                    m0 = dialog.getMaxM0();
                                    t2 = dialog.getMaxT2();
                                }
                            }
                            else {
                                m0 = dialog.getMaxM0();
                                t2 = dialog.getMaxT2();
                            }
                            
                            
                            if (t2 < 0.00 || t2 > dialog.getMaxT2()) {
                                t2 = dialog.getMaxT2();
                            }
                            if (m0 < 0.00 || m0 > dialog.getMaxM0()) {
                                m0 = dialog.getMaxM0();
                            }
                            
                            // invert to r2
                            if (t2 != 0.00) {
                                r2 = 1.00/t2;
                            }
                            else {
                                r2 = 0.00;
                            }
                        
                        
                            if (dialog.isCalculateT2()) { 
                                t2Values[k][pixelIndex] = (float) t2;
                            }
                            if (dialog.isCalculateM0()) { 
                                m0Values[k][pixelIndex] = (float) m0;
                            }
                            if (dialog.isInvertT2toR2()) { 
                                r2Values[k][pixelIndex] = (float) r2;
                            }
                        }
                        else {
                            if (dialog.isCalculateT2()) { 
                                t2Values[k][pixelIndex] = (float) 0.00;
                            }
                            if (dialog.isCalculateM0()) { 
                                m0Values[k][pixelIndex] = (float) 0.00;
                            }
                            if (dialog.isInvertT2toR2()) {
                                r2Values[k][pixelIndex] = (float) 0.00;
                            }
                        }
                        pixelIndex++;
                    }
                }
                
                try {
                    int startVal = image.getSliceSize()*nSlices*t + image.getSliceSize()*k;
                    if (dialog.isCalculateT2()) {
                        t2ResultStack.importData(startVal, t2Values[k], true);
                    }
                    if (dialog.isCalculateM0()) {
                        m0ResultStack.importData(startVal, m0Values[k], true);
                    }
                    if (dialog.isInvertT2toR2()) {
                        r2ResultStack.importData(startVal, r2Values[k], true);
                    }
                } catch(IOException e) {
                    e.printStackTrace();
                    MipavUtil.displayError("Data could not be imported into result image");
                }
            }
        }
        
        if (dialog.isCalculateT2()) {
            t2ResultWindow = new ViewJFrameImage(t2ResultStack);
            t2ResultWindow.setTitle("TreT2-T2_Map");
            t2ResultWindow.setVisible(true);
        } 
        
        if (dialog.isCalculateM0()) {
            m0ResultWindow = new ViewJFrameImage(m0ResultStack);
            m0ResultWindow.setTitle("TreT2_M0Map");
            m0ResultWindow.setVisible(true);
        } 
        
        if (dialog.isInvertT2toR2()) {
            r2ResultWindow = new ViewJFrameImage(r2ResultStack);
            r2ResultWindow.setTitle("TreT2-R2Map");
            r2ResultWindow.setVisible(true);
        }   
    }
    
    public void calculateT2with180Phase() {
        ModelImage image;
        
        double[] fa_phase180;
        double[] scaledFA_phase180;
        
        double[][] ssfpPixelValues_phase180;
        double[] t1PixelValues, b1PixelValues;
        double[] phase180Data;
        
        float[][] t2Values, m0Values, r2Values;
        
        double a, d, e2;
        double sumX, sumY, sumXY, sumXX, slope, denominator, intercept, t2, e1, m0, r2;
        
        int width, height, nSlices, tSeries;
        int x,y,k,t,angle, p, pixelIndex;

        image = ViewUserInterface.getReference().getRegisteredImageByName(wList[ssfpImageIndex_phase180[0]]);
        width = image.getExtents()[0];
        height = image.getExtents()[1];
        if(image.getNDims() > 2) {
            nSlices = image.getExtents()[2];
        } else {
            nSlices = 1;
        }

        boolean do4D = false;
        tSeries = 1;
        for (angle=0; angle<dialog.getNfa_phase180(); angle++) {
            image = ViewUserInterface.getReference().getRegisteredImageByName(wList[ssfpImageIndex_phase180[angle]]);
            if(image.getNDims() > 3 && !do4D) { //clause is only entered once
                do4D = true;
                tSeries = image.getExtents()[3];
                t2ResultStack = new ModelImage(ModelImage.DOUBLE, image.getExtents(), "t2_results");
                m0ResultStack = new ModelImage(ModelImage.DOUBLE, image.getExtents(), "m0_results");
                r2ResultStack = new ModelImage(ModelImage.DOUBLE, image.getExtents(), "r2_results");
                
                t2ResultStack = nearCloneImage(image, t2ResultStack);
                m0ResultStack = nearCloneImage(image, m0ResultStack);
                r2ResultStack = nearCloneImage(image, r2ResultStack);
            }
        }
        
        if(!do4D) {
            t2ResultStack = new ModelImage(ModelImage.DOUBLE, image.getExtents(), "t2_results");
            m0ResultStack = new ModelImage(ModelImage.DOUBLE, image.getExtents(), "m0_results");
            r2ResultStack = new ModelImage(ModelImage.DOUBLE, image.getExtents(), "r2_results");
            
            t2ResultStack = nearCloneImage(image, t2ResultStack);
            m0ResultStack = nearCloneImage(image, m0ResultStack);
            r2ResultStack = nearCloneImage(image, r2ResultStack);
        }
        
        String prefix = new String();
        // Actually perform the T2 Calculations
        for(t=0; t<tSeries; t++) {
            if(do4D) {
                prefix = "Series "+t+": ";
            } else {
                prefix = "";
            }
            
            ssfpPixelValues_phase180 = new double[dialog.getNfa_phase180()][width*height];
            t1PixelValues = new double[width*height];
            if (dialog.isIncludeB1Map()) { 
                b1PixelValues = new double[width*height];
            }
            else { 
                b1PixelValues = new double[1];
            }
            
            if (dialog.isCalculateT2()) { 
                t2Values = new float[nSlices][width*height];
            }
            else { 
                t2Values = new float[1][1];
            }
            if (dialog.isCalculateM0()) { 
                m0Values = new float[nSlices][width*height];
            }
            else { 
                m0Values = new float[1][1];
            }
            if (dialog.isInvertT2toR2()) { 
                r2Values = new float[nSlices][width*height];
            }
            else { 
                r2Values = new float[1][1];
            }
            
            fa_phase180 = new double[dialog.getNfa_phase180()];
            scaledFA_phase180 = new double[dialog.getNfa_phase180()];
            for (angle=0; angle<dialog.getNfa_phase180(); angle++) {
                fa_phase180[angle] = Math.toRadians(treFA_phase180[angle]);
            }
            
            phase180Data = new double[dialog.getNfa_phase180()];
            
            for (k=0; k<nSlices; k++) {
                fireProgressStateChanged(prefix+"calculating T2 values on slice: "+k+" of "+(nSlices-1));
                fireProgressStateChanged(0+(int)((float)k/(float)nSlices*80.0));
                if(interrupted()) {
                    hardInterrupt = true;
                    return;
                }
                // grab the ssfp pixel values from the phase = 180 data
                for (angle=0; angle<dialog.getNfa_phase180(); angle++) {
                    image = ViewUserInterface.getReference().getRegisteredImageByName(wList[ssfpImageIndex_phase180[angle]]);
                    
                    pixelIndex = 0;
                    for (y=0; y<height; y++) {
                        for (x=0; x<width; x++) {
                            if(image.getNDims() < 4) {
                                ssfpPixelValues_phase180[angle][pixelIndex] = image.getDouble(x, y, k);
                            } else {
                                ssfpPixelValues_phase180[angle][pixelIndex] = image.getDouble(x, y, k, t);
                            }
                            pixelIndex++;
                        }
                    }
                }
                
                // grab the T1 and B1 information
                image = ViewUserInterface.getReference().getRegisteredImageByName(wList[t1ImageIndex]);
                
                pixelIndex = 0;
                for (y=0; y<height; y++) {
                    for (x=0; x<width; x++) {
                        if(image.getNDims() < 4) {
                            t1PixelValues[pixelIndex] = image.getDouble(x, y, k);
                        } else {
                            t1PixelValues[pixelIndex] = image.getDouble(x, y, k, t);
                        }
                        pixelIndex++;
                    }
                }
            
                if (dialog.isIncludeB1Map() == true) {
                    image = ViewUserInterface.getReference().getRegisteredImageByName(wList[b1ImageIndex]);
                    
                    pixelIndex = 0;
                    for (y=0; y<height; y++) {
                        for (x=0; x<width; x++) {
                            if(image.getNDims() < 4) {    
                                b1PixelValues[pixelIndex] = image.getDouble(x,y, k);
                            } else {
                                b1PixelValues[pixelIndex] = image.getDouble(x,y, k, t);
                            }
                            pixelIndex++;
                        }
                    }
                }
            
                // now that we have all the information, perform the calculate pixel-wise
                pixelIndex = 0;
                for (x=0; x<width; x++) {
                    for (y=0; y<height; y++) {
                        
                        if (t1PixelValues[pixelIndex] > 0.00) {
                            
                            e1 = Math.exp(-dialog.getTreTR()/t1PixelValues[pixelIndex]);
                            
                            // scale up (or down) the flip angles based on the calculated B1 if required
                            if (dialog.isIncludeB1Map() == true) {
                                for (p=0; p<dialog.getNfa_phase180(); p++) { 
                                    scaledFA_phase180[p] = fa_phase180[p]*b1PixelValues[pixelIndex];
                                }
                            }
                            
                            else {
                                for (p=0; p<dialog.getNfa_phase180(); p++) { 
                                    scaledFA_phase180[p] = fa_phase180[p];
                                }
                            }
                            
                            // grab the SSFP values for this pixel
                            for (p=0; p<dialog.getNfa_phase180(); p++) { 
                                phase180Data[p] = ssfpPixelValues_phase180[p][pixelIndex];
                            }
                            
                            intercept = 1.00;
                            denominator = 1.00;
                            slope = 0.00;
                            e2 = 1.00;
                            t2 = 0.00;
                            
                            // calculate T2 first from the phase = 180 data
                            sumX = 0.00;
                            sumY = 0.00;
                            sumXY = 0.00;
                            sumXX = 0.00;
                        
                            for (p=0; p<dialog.getNfa_phase180(); p++) {
                                sumX += phase180Data[p]/Math.tan(scaledFA_phase180[p]);
                                sumY += phase180Data[p]/Math.sin(scaledFA_phase180[p]);
                                sumXY += (phase180Data[p]/Math.tan(scaledFA_phase180[p]))*(phase180Data[p]/Math.sin(scaledFA_phase180[p]));
                                sumXX += (phase180Data[p]/Math.tan(scaledFA_phase180[p]))*(phase180Data[p]/Math.tan(scaledFA_phase180[p]));
                            }
                            
                            d = (dialog.getNfa_phase180()*sumXX) - sumX*sumX;
                            a = (dialog.getNfa_phase180()*sumXY) - (sumX*sumY);
                            
                            if (d != 0) {
                                slope = a/d;
                                denominator = (slope-e1)/(slope*e1-1.00);
                                intercept = (sumY-slope*sumX)/dialog.getNfa_phase180();
                                
                                if (denominator > 0.00 && denominator < 1.00) {
                                    t2 = -dialog.getTreTR()/Math.log(denominator);
                                    e2 = Math.exp(-dialog.getTreTR()/t2);
                                    m0 = intercept*(1.00-e1*e2)/(1.00-e1);
                                }
                                else {
                                    m0 = dialog.getMaxM0();
                                    t2 = dialog.getMaxT2();
                                }
                            }
                            else {
                                m0 = dialog.getMaxM0();
                                t2 = dialog.getMaxT2();
                            }
                        
                        
                            if (t2 < 0.00 || t2 > dialog.getMaxT2()) {
                                t2 = dialog.getMaxT2();
                            }
                            if (m0 < 0.00 || m0 > dialog.getMaxM0()) {
                                m0 = dialog.getMaxM0();
                            }
                                                    
                            // invert to r2
                            if (t2 != 0.00) {
                                r2 = 1.00/t2;
                            }
                            else {
                                r2 = 0.00;
                            }
                            
                            
                            if (dialog.isCalculateT2()) t2Values[k][pixelIndex] = (float) t2;
                            if (dialog.isCalculateM0()) m0Values[k][pixelIndex] = (float) m0;
                            if (dialog.isInvertT2toR2()) r2Values[k][pixelIndex] = (float) r2;
                        }
                        else {
                            if (dialog.isCalculateT2()) t2Values[k][pixelIndex] = (float) 0.00;
                            if (dialog.isCalculateM0()) m0Values[k][pixelIndex] = (float) 0.00;
                            if (dialog.isInvertT2toR2()) r2Values[k][pixelIndex] = (float) 0.00;
                        }
                        pixelIndex++;
                    }
                }
            
                try {
                    int startVal = image.getSliceSize()*nSlices*t + image.getSliceSize()*k;
                    if (dialog.isCalculateT2()) { 
                        t2ResultStack.importData(startVal, t2Values[k], true);
                    }
                    if (dialog.isCalculateM0()) { 
                        m0ResultStack.importData(startVal, m0Values[k], true);
                    }
                    if (dialog.isInvertT2toR2()) { 
                        r2ResultStack.importData(startVal, r2Values[k], true);
                    }
                } catch(IOException e) {
                    e.printStackTrace();
                    MipavUtil.displayError("Data could not be imported into result image");
                }
            }
        }
        
        if (dialog.isCalculateT2()) {
            t2ResultWindow = new ViewJFrameImage(t2ResultStack);
            t2ResultWindow.setTitle("TreT2_T2_Map");
            t2ResultWindow.setVisible(true);
        } 
        
        if (dialog.isCalculateM0()) {
            m0ResultWindow = new ViewJFrameImage(m0ResultStack);
            m0ResultWindow.setTitle("TreT2_M0Map");
            m0ResultWindow.setVisible(true);
        } 
        
        if (dialog.isInvertT2toR2()) {
            r2ResultWindow = new ViewJFrameImage(r2ResultStack);
            r2ResultWindow.setTitle("TreT2_R2Map");
            r2ResultWindow.setVisible(true);
        } 
    }
    
    

    public void calculateT2withApproximateModelling() {
        ModelImage image;
        
        double[] fa_phase0, fa_phase180;
        double[] scaledFA_phase0, scaledFA_phase180;
        
        double[][] ssfpPixelValues_phase0, ssfpPixelValues_phase180;
        double[] t1PixelValues, b1PixelValues;
        double[] phase0Data, phase180Data;
        
        float[][] t2Values, m0Values, r2Values;
        
        double a, d, e2;
        double sumX, sumY, sumXY, sumXX, slope, denominator, intercept, t2, e1, m0, r2;
        double[] possibleT2s, possibleM0s;
        
        int width, height, nSlices, tSeries;
        int x,y,k,t,angle, p, pixelIndex;

        image = ViewUserInterface.getReference().getRegisteredImageByName(wList[ssfpImageIndex_phase0[0]]);
        width = image.getExtents()[0];
        height = image.getExtents()[1];
        if(image.getNDims() > 2) {
            nSlices = image.getExtents()[2];
        } else {
            nSlices = 1;
        }
        
        boolean do4D = false;
        tSeries = 1;
        for (angle=0; angle<dialog.getNfa_phase0(); angle++) {
            image = ViewUserInterface.getReference().getRegisteredImageByName(wList[ssfpImageIndex_phase0[angle]]);
            if(image.getNDims() > 3 && !do4D) { //clause is only entered once
                do4D = true;
                tSeries = image.getExtents()[3];
                t2ResultStack = new ModelImage(ModelImage.DOUBLE, image.getExtents(), "t2_results");
                m0ResultStack = new ModelImage(ModelImage.DOUBLE, image.getExtents(), "m0_results");
                r2ResultStack = new ModelImage(ModelImage.DOUBLE, image.getExtents(), "r2_results");
                
                t2ResultStack = nearCloneImage(image, t2ResultStack);
                m0ResultStack = nearCloneImage(image, m0ResultStack);
                r2ResultStack = nearCloneImage(image, r2ResultStack);
            }
        }
        
        if(!do4D) {
            for (angle=0; angle<dialog.getNfa_phase180(); angle++) {
                image = ViewUserInterface.getReference().getRegisteredImageByName(wList[ssfpImageIndex_phase180[angle]]);
                if(image.getNDims() > 3 && !do4D) { //clause is only entered once
                    do4D = true;
                    tSeries = image.getExtents()[3];
                    t2ResultStack = new ModelImage(ModelImage.DOUBLE, image.getExtents(), "t2_results");
                    m0ResultStack = new ModelImage(ModelImage.DOUBLE, image.getExtents(), "m0_results");
                    r2ResultStack = new ModelImage(ModelImage.DOUBLE, image.getExtents(), "r2_results");
                    
                    t2ResultStack = nearCloneImage(image, t2ResultStack);
                    m0ResultStack = nearCloneImage(image, m0ResultStack);
                    r2ResultStack = nearCloneImage(image, r2ResultStack);
                }
            }
        }
        
        if(!do4D) {
            t2ResultStack = new ModelImage(ModelImage.DOUBLE, image.getExtents(), "t2_results");
            m0ResultStack = new ModelImage(ModelImage.DOUBLE, image.getExtents(), "m0_results");
            r2ResultStack = new ModelImage(ModelImage.DOUBLE, image.getExtents(), "r2_results");
            
            t2ResultStack = nearCloneImage(image, t2ResultStack);
            m0ResultStack = nearCloneImage(image, m0ResultStack);
            r2ResultStack = nearCloneImage(image, r2ResultStack);
        }
        
        String prefix = new String();
        // Actually perform the T2 Calculations
        for(t=0; t<tSeries; t++) {
            if(do4D) {
                prefix = "Series "+t+": ";
            } else {
                prefix = "";
            }
            
            ssfpPixelValues_phase0 = new double[dialog.getNfa_phase0()][width*height];
            ssfpPixelValues_phase180 = new double[dialog.getNfa_phase180()][width*height];
            t1PixelValues = new double[width*height];
            if (dialog.isIncludeB1Map()) { 
                b1PixelValues = new double[width*height];
            }
            else { 
                b1PixelValues = new double[1];
            }
            
            if (dialog.isCalculateT2()) { 
                t2Values = new float[nSlices][width*height];
            }
            else t2Values = new float[1][1];
            if (dialog.isCalculateM0()) { 
                m0Values = new float[nSlices][width*height];
            }
            else m0Values = new float[1][1];
            if (dialog.isInvertT2toR2()) { 
                r2Values = new float[nSlices][width*height];
            }
            else { 
                r2Values = new float[1][1];
            }
            
            fa_phase0 = new double[dialog.getNfa_phase0()];
            fa_phase180 = new double[dialog.getNfa_phase180()];
            scaledFA_phase0 = new double[dialog.getNfa_phase0()];
            scaledFA_phase180 = new double[dialog.getNfa_phase180()];
            for (angle=0; angle<dialog.getNfa_phase0(); angle++) {
                fa_phase0[angle] = Math.toRadians(treFA_phase0[angle]);
            }
            for (angle=0; angle<dialog.getNfa_phase180(); angle++) {
                fa_phase180[angle] = Math.toRadians(treFA_phase180[angle]);
            }
            
            phase0Data = new double[dialog.getNfa_phase0()];
            phase180Data = new double[dialog.getNfa_phase180()];
            
            possibleT2s = new double[2];
            possibleM0s = new double[2];
            
            for (k=0; k<nSlices; k++) {
                fireProgressStateChanged(prefix+"calculating T2 values on slice: "+k+" of "+(nSlices-1));
                fireProgressStateChanged(0+(int)((float)k/(float)nSlices*80.0));
                if(interrupted()) {
                    hardInterrupt = true;
                    return;
                }
                // grab the ssfp pixel values from the phase = 0 data
                for (angle=0; angle<dialog.getNfa_phase0(); angle++) {
                    image = ViewUserInterface.getReference().getRegisteredImageByName(wList[ssfpImageIndex_phase0[angle]]);
                    
                    pixelIndex = 0;
                    for (y=0; y<height; y++) {
                        for (x=0; x<width; x++) {
                            if(image.getNDims() < 4) {
                                ssfpPixelValues_phase0[angle][pixelIndex] = image.getDouble(x, y, k);
                            } else {
                                ssfpPixelValues_phase0[angle][pixelIndex] = image.getDouble(x, y, k, t);
                            }
                            pixelIndex++;
                        }
                    }
                }
            
                // grab the ssfp pixel values from the phase = 180 data
                for (angle=0; angle<dialog.getNfa_phase180(); angle++) {
                    image = ViewUserInterface.getReference().getRegisteredImageByName(wList[ssfpImageIndex_phase180[angle]]);
                    
                    pixelIndex = 0;
                    for (y=0; y<height; y++) {
                        for (x=0; x<width; x++) {
                            if(image.getNDims() < 4) {
                                ssfpPixelValues_phase180[angle][pixelIndex] = image.getDouble(x, y, k);
                            } else {
                                ssfpPixelValues_phase180[angle][pixelIndex] = image.getDouble(x, y, k, t);
                            }
                            pixelIndex++;
                        }
                    }
                }
                
                // grab the T1 and B1 information
                image = ViewUserInterface.getReference().getRegisteredImageByName(wList[t1ImageIndex]);
                
                pixelIndex = 0;
                for (y=0; y<height; y++) {
                    for (x=0; x<width; x++) {
                        if(image.getNDims() < 4) {
                            t1PixelValues[pixelIndex] = image.getDouble(x, y, k);
                        } else {
                            t1PixelValues[pixelIndex] = image.getDouble(x, y, k, t);
                        }
                        pixelIndex++;
                    }
                }
            
                if (dialog.isIncludeB1Map() == true) {
                    image = ViewUserInterface.getReference().getRegisteredImageByName(wList[b1ImageIndex]);
                    
                    pixelIndex = 0;
                    for (y=0; y<height; y++) {
                        for (x=0; x<width; x++) {
                            if(image.getNDims() < 4) {    
                                b1PixelValues[pixelIndex] = image.getDouble(x,y, k);
                            } else {
                                b1PixelValues[pixelIndex] = image.getDouble(x,y, k, t);
                            }
                            pixelIndex++;
                        }
                    }
                }
                
                // now that we have all the information, perform the calculate pixel-wise
                pixelIndex = 0;
                for (x=0; x<width; x++) {
                    for (y=0; y<height; y++) {
                    
                        if (t1PixelValues[pixelIndex] > 0.00) {
                            
                            e1 = Math.exp(-dialog.getTreTR()/t1PixelValues[pixelIndex]);
                            
                            // scale up (or down) the flip angles based on the calculated B1 if required
                            if (dialog.isIncludeB1Map() == true) {
                                for (p=0; p<dialog.getNfa_phase0(); p++) scaledFA_phase0[p] = fa_phase0[p]*b1PixelValues[pixelIndex];
                                for (p=0; p<dialog.getNfa_phase180(); p++) scaledFA_phase180[p] = fa_phase180[p]*b1PixelValues[pixelIndex];
                            }
                            else {
                                for (p=0; p<dialog.getNfa_phase0(); p++) {
                                    scaledFA_phase0[p] = fa_phase0[p];
                                    scaledFA_phase180[p] = fa_phase180[p];
                                }
                            }
                        
                            // grab the SSFP values for this pixel
                            for (p=0; p<dialog.getNfa_phase0(); p++) phase0Data[p] = ssfpPixelValues_phase0[p][pixelIndex];
                            for (p=0; p<dialog.getNfa_phase180(); p++) phase180Data[p] = ssfpPixelValues_phase180[p][pixelIndex];
                            
                            intercept = 1.00;
                            denominator = 1.00;
                            slope = 0.00;
                            e2 = 1.00;
                            t2 = 0.00;
                            
                            // calculate T2 first from the phase = 0 data
                            sumX = 0.00;
                            sumY = 0.00;
                            sumXY = 0.00;
                            sumXX = 0.00;
                        
                            for (p=0; p<dialog.getNfa_phase0(); p++) {
                                sumX += phase0Data[p]/Math.tan(scaledFA_phase0[p]);
                                sumY += phase0Data[p]/Math.sin(scaledFA_phase0[p]);
                                sumXY += (phase0Data[p]/Math.tan(scaledFA_phase0[p]))*(phase0Data[p]/Math.sin(scaledFA_phase0[p]));
                                sumXX += (phase0Data[p]/Math.tan(scaledFA_phase0[p]))*(phase0Data[p]/Math.tan(scaledFA_phase0[p]));
                            }
                            
                            d = (dialog.getNfa_phase0()*sumXX) - sumX*sumX;
                            a = (dialog.getNfa_phase0()*sumXY) - (sumX*sumY);
                            
                            if (d != 0) {
                                slope = a/d;
                                denominator = (e1-slope)/(1.00-slope*e1);
                                intercept = (sumY-slope*sumX)/dialog.getNfa_phase0();
                                
                                if (denominator > 0.00 && denominator < 1.00) {
                                    t2 = -dialog.getTreTR()/Math.log(denominator);
                                    e2 = Math.exp(-dialog.getTreTR()/t2);
                                    m0 = intercept*(1.00-e1*e2)/(1.00-e1);
                                }
                                else {
                                    m0 = 0.00;
                                    t2 = 0.00;
                                }
                            }
                            else {
                                m0 = 0.00;
                                t2 = 0.00;
                            }
                            
    
                            if (t2 < 0.00 || t2 > dialog.getMaxT2()) {
                                t2 = dialog.getMaxT2();
                            }
                            if (m0 < 0.00 || m0 > dialog.getMaxM0()) {
                                m0 = 0.00;
                            }
                        
                
                            possibleT2s[0] = t2;
                            possibleM0s[0] = m0;
                            
                            
                            // calculate T2 first from the phase = 180 data
                            sumX = 0.00;
                            sumY = 0.00;
                            sumXY = 0.00;
                            sumXX = 0.00;
                            
                            for (p=0; p<dialog.getNfa_phase180(); p++) {
                                sumX += phase180Data[p]/Math.tan(scaledFA_phase180[p]);
                                sumY += phase180Data[p]/Math.sin(scaledFA_phase180[p]);
                                sumXY += (phase180Data[p]/Math.tan(scaledFA_phase180[p]))*(phase180Data[p]/Math.sin(scaledFA_phase180[p]));
                                sumXX += (phase180Data[p]/Math.tan(scaledFA_phase180[p]))*(phase180Data[p]/Math.tan(scaledFA_phase180[p]));
                            }
                            
                            d = (dialog.getNfa_phase180()*sumXX) - sumX*sumX;
                            a = (dialog.getNfa_phase180()*sumXY) - (sumX*sumY);
                        
                            if (d != 0) {
                                slope = a/d;
                                denominator = (slope-e1)/(slope*e1-1.00);
                                intercept = (sumY-slope*sumX)/dialog.getNfa_phase180();
                                
                                if (denominator > 0.00 && denominator < 1.00) {
                                    t2 = -dialog.getTreTR()/Math.log(denominator);
                                    e2 = Math.exp(-dialog.getTreTR()/t2);
                                    m0 = intercept*(1.00-e1*e2)/(1.00-e1);
                                }
                                else {
                                    m0 = 0.00;
                                    t2 = 0.00;
                                }
                            }
                            else {
                                m0 = 0.00;
                                t2 = 0.00;
                            }
                            
                            
                            if (t2 < 0.00 || t2 > dialog.getMaxT2()) {
                                t2 = dialog.getMaxT2();
                            }
                            if (m0 < 0.00 || m0 > dialog.getMaxM0()) {
                                m0 = 0.00;
                            }
                            
                            possibleT2s[1] = t2;
                            possibleM0s[1] = m0;
                        
                            // now, choose the maximum T2 and the corresponding m0 value
                            if (possibleT2s[0] >= possibleT2s[1]) {
                                t2 = possibleT2s[0];
                                m0 = possibleM0s[0];
                            }
                            else {
                                t2 = possibleT2s[1];
                                m0 = possibleM0s[1];
                            }
                             
                            
                            //t2 = (possibleT2s[0]+possibleT2s[1])/2.00;
                            //m0 = (possibleM0s[0]+possibleM0s[1])/2.00;
                        
                        
                            // invert to r2
                            if (t2 != 0.00) {
                                r2 = 1.00/t2;
                            }
                            else {
                                r2 = 0.00;
                            }
                                            
                            if (dialog.isCalculateT2()) { 
                                t2Values[k][pixelIndex] = (float) t2;
                            }
                            if (dialog.isCalculateM0()) { 
                                m0Values[k][pixelIndex] = (float) m0;
                            }
                            if (dialog.isInvertT2toR2()) { 
                                r2Values[k][pixelIndex] = (float) r2;
                            }
                        }
                        else {
                            if (dialog.isCalculateT2()) { 
                                t2Values[k][pixelIndex] = (float) 0.00;
                            }
                            if (dialog.isCalculateM0()) { 
                                m0Values[k][pixelIndex] = (float) 0.00;
                            }
                            if (dialog.isInvertT2toR2()) { 
                                r2Values[k][pixelIndex] = (float) 0.00;
                            }
                        }
                        pixelIndex++;
                    }
                }
                
                try {
                    int startVal = image.getSliceSize()*nSlices*t + image.getSliceSize()*k;
                    if (dialog.isCalculateT2()) { 
                        t2ResultStack.importData(startVal, t2Values[k], true);
                    }
                    if (dialog.isCalculateM0()) { 
                        m0ResultStack.importData(startVal, m0Values[k], true);
                    }
                    if (dialog.isInvertT2toR2()) { 
                        r2ResultStack.importData(startVal, r2Values[k], true);
                    }
                } catch(IOException e) {
                    e.printStackTrace();
                    MipavUtil.displayError("Data could not be imported into result image");
                }
            }
        }
        
        if (dialog.isCalculateT2()) {
            t2ResultWindow = new ViewJFrameImage(t2ResultStack);
            t2ResultWindow.setTitle("CalculatedT2Map_AM");
            t2ResultWindow.setVisible(true);
        } 
        
        if (dialog.isCalculateM0()) {
            m0ResultWindow = new ViewJFrameImage(m0ResultStack);
            m0ResultWindow.setTitle("CalculatedM0Map_AM");
            m0ResultWindow.setVisible(true);
        } 
        
        if (dialog.isInvertT2toR2()) {
            r2ResultWindow = new ViewJFrameImage(r2ResultStack);
            r2ResultWindow.setTitle("CalculatedR2Map_AM");
            r2ResultWindow.setVisible(true);
        } 
    }
    
    public void calculateT2withFullModelling() {
        fireProgressStateChanged("prepping data - hang on");
        ModelImage image;
        
        double[] FA, scaledFA, phaseIncrements, sina, cosa;
        
        double[][] ssfpPixelValues_phase0, ssfpPixelValues_phase180;
        double[] t1PixelValues, b1PixelValues;
        double[] ssfpSampleData;
        
        float[][] b0Field, m0Field, t2Field, r2Field;
        double[][][] t2Values, m0Values, r2Values, b0Values;
        double smoothedB0;
        
        double[] optimization, initialGuess;
        double[] twoPOptimization, twoPInitialGuess;
        int numParams, numVertices;
        
        @SuppressWarnings("unused")
        double resonancePeriod;
        
        optimization = new double[3];
        initialGuess = new double[3];
        twoPOptimization = new double[2];
        twoPInitialGuess = new double[2];
        
        double t2, m0, b0, r2;
        
        double t1, tr;
        
        int width, height, nSlices, tSeries;
        int x,y,k,t,angle, p, pixelIndex, p1,p2;
        
        image = ViewUserInterface.getReference().getRegisteredImageByName(wList[ssfpImageIndex_phase180[0]]);
        width = image.getExtents()[0];
        height = image.getExtents()[1];
        if(image.getNDims() > 2) {
            nSlices = image.getExtents()[2];
        } else {
            nSlices = 1;
        }
        
        boolean do4D = false;
        tSeries = 1;
        for (angle=0; angle<dialog.getNfa_phase180(); angle++) {
            image = ViewUserInterface.getReference().getRegisteredImageByName(wList[ssfpImageIndex_phase180[angle]]);
            if(image.getNDims() > 3 && !do4D) { //clause is only entered once
                do4D = true;
                tSeries = image.getExtents()[3];
                t2ResultStack = new ModelImage(ModelImage.DOUBLE, image.getExtents(), "t2_results");
                m0ResultStack = new ModelImage(ModelImage.DOUBLE, image.getExtents(), "m0_results");
                r2ResultStack = new ModelImage(ModelImage.DOUBLE, image.getExtents(), "r2_results");
                b0ResultStack = new ModelImage(ModelImage.DOUBLE, image.getExtents(), "b0_results");
                
                t2ResultStack = nearCloneImage(image, t2ResultStack);
                m0ResultStack = nearCloneImage(image, m0ResultStack);
                r2ResultStack = nearCloneImage(image, r2ResultStack);
                b0ResultStack = nearCloneImage(image, b0ResultStack);
            }
        }
        
        if(!do4D) {
            for (angle=0; angle<dialog.getNfa_phase0(); angle++) {
                image = ViewUserInterface.getReference().getRegisteredImageByName(wList[ssfpImageIndex_phase0[angle]]);
                if(image.getNDims() > 3 && !do4D) { //clause is only entered once
                    do4D = true;
                    tSeries = image.getExtents()[3];
                    t2ResultStack = new ModelImage(ModelImage.DOUBLE, image.getExtents(), "t2_results");
                    m0ResultStack = new ModelImage(ModelImage.DOUBLE, image.getExtents(), "m0_results");
                    r2ResultStack = new ModelImage(ModelImage.DOUBLE, image.getExtents(), "r2_results");
                    b0ResultStack = new ModelImage(ModelImage.DOUBLE, image.getExtents(), "b0_results");
                    
                    t2ResultStack = nearCloneImage(image, t2ResultStack);
                    m0ResultStack = nearCloneImage(image, m0ResultStack);
                    r2ResultStack = nearCloneImage(image, r2ResultStack);
                    b0ResultStack = nearCloneImage(image, b0ResultStack);
                }
            }
        }

        if(!do4D) {
            t2ResultStack = new ModelImage(ModelImage.DOUBLE, image.getExtents(), "t2_results");
            m0ResultStack = new ModelImage(ModelImage.DOUBLE, image.getExtents(), "m0_results");
            r2ResultStack = new ModelImage(ModelImage.DOUBLE, image.getExtents(), "r2_results");
            b0ResultStack = new ModelImage(ModelImage.DOUBLE, image.getExtents(), "b0_results");
            
            t2ResultStack = nearCloneImage(image, t2ResultStack);
            m0ResultStack = nearCloneImage(image, m0ResultStack);
            r2ResultStack = nearCloneImage(image, r2ResultStack);
            b0ResultStack = nearCloneImage(image, b0ResultStack);
        }

        String prefix = new String();
        // Perform an initial T2 Calculation to get the rough b0 field
        for(t=0; t<tSeries; t++) {
            if(do4D) {
                prefix = "Series "+t+": ";
            } else {
                prefix = "";
            }   
            
            ssfpPixelValues_phase0 = new double[dialog.getNfa_phase0()][width*height];
            ssfpPixelValues_phase180 = new double[dialog.getNfa_phase180()][width*height];
            t1PixelValues = new double[width*height];
            if (dialog.isIncludeB1Map()) { 
                b1PixelValues = new double[width*height];
            }
            else { 
                b1PixelValues = new double[1];
            }
            
            FA = new double[dialog.getNfa_phase0() + dialog.getNfa_phase180()];
            scaledFA = new double[dialog.getNfa_phase0() + dialog.getNfa_phase180()];
            phaseIncrements = new double[dialog.getNfa_phase0() + dialog.getNfa_phase180()];
            sina = new double[dialog.getNfa_phase0() + dialog.getNfa_phase180()];
            cosa = new double[dialog.getNfa_phase0() + dialog.getNfa_phase180()];
            
            for (angle=0; angle<dialog.getNfa_phase0(); angle++) {
                FA[angle] = Math.toRadians(treFA_phase0[angle]);
                phaseIncrements[angle] = 0.00;
            }
            for (angle=0; angle<dialog.getNfa_phase180(); angle++) {
                FA[angle+dialog.getNfa_phase0()] = Math.toRadians(treFA_phase180[angle]);
                phaseIncrements[angle+dialog.getNfa_phase0()] = 3.14159265;
            }
            
            ssfpSampleData = new double[dialog.getNfa_phase0() + dialog.getNfa_phase180()];
            
            tr = dialog.getTreTR();
            resonancePeriod = 1000.00/tr;
            
            t2Values = new double[nSlices][height][width];
            m0Values = new double[nSlices][height][width];
            b0Values = new double[nSlices][height][width];
            r2Values = new double[nSlices][height][width];
            
            b0Field = new float[nSlices][width*height];
            m0Field = new float[nSlices][width*height];
            t2Field = new float[nSlices][width*height];
            r2Field = new float[nSlices][width*height];
            
            // simplex - specific parameters
            numParams = 3;
            numVertices = numParams+1;
            simplex = new double[numVertices][numVertices];
            simplexLineValues = new double[numParams];
            simplexResiduals = new double[numVertices];
            bestToWorst = new int[3];
            
            simplexCentre = new double[numParams];
            reflection = new double[numVertices];
            expansion = new double[numVertices];
            contraction = new double[numVertices];
            shrink = new double[numVertices];
            
            
            numParams = 2;
            numVertices = numParams+1;
            twoPSimplex = new double[numVertices][numVertices];
            twoPSimplexLineValues = new double[numParams];
            twoPSimplexResiduals = new double[numVertices];
            bestToWorst = new int[3];
            
            twoPSimplexCentre = new double[numParams];
            twoPReflection = new double[numVertices];
            twoPExpansion = new double[numVertices];
            twoPContraction = new double[numVertices];
            twoPShrink = new double[numVertices];
        
            for (k=0; k<nSlices; k++) {
                fireProgressStateChanged(prefix+"calculating Initial Estimates for slice: "+k+" of "+(nSlices-1));
                fireProgressStateChanged(0+(int)((float)k/(float)nSlices*40.0));
                if(interrupted()) {
                    hardInterrupt = true;
                    return;
                }
                // grab the ssfp pixel values from the phase = 0 data
                for (angle=0; angle<dialog.getNfa_phase0(); angle++) {
                    image = ViewUserInterface.getReference().getRegisteredImageByName(wList[ssfpImageIndex_phase0[angle]]);
                    
                    pixelIndex = 0;
                    for (y=0; y<height; y++) {
                        for (x=0; x<width; x++) {
                            if(image.getNDims() < 4) {
                                ssfpPixelValues_phase0[angle][pixelIndex] = image.getDouble(x, y, k);
                            } else {
                                ssfpPixelValues_phase0[angle][pixelIndex] = image.getDouble(x, y, k, t);
                            }
                            pixelIndex++;
                        }
                    }
                }
            
                // grab the ssfp pixel values from the phase = 180 data
                for (angle=0; angle<dialog.getNfa_phase180(); angle++) {
                    image = ViewUserInterface.getReference().getRegisteredImageByName(wList[ssfpImageIndex_phase180[angle]]);
                    
                    pixelIndex = 0;
                    for (y=0; y<height; y++) {
                        for (x=0; x<width; x++) {
                            if(image.getNDims() < 4) {
                                ssfpPixelValues_phase180[angle][pixelIndex] = image.getDouble(x, y, k);
                            } else {
                                ssfpPixelValues_phase180[angle][pixelIndex] = image.getDouble(x, y, k, t);
                            }
                            pixelIndex++;
                        }
                    }
                }
                
                // grab the T1 and B1 information
                image = ViewUserInterface.getReference().getRegisteredImageByName(wList[t1ImageIndex]);
                
                pixelIndex = 0;
                for (y=0; y<height; y++) {
                    for (x=0; x<width; x++) {
                        if(image.getNDims() < 4) {
                            t1PixelValues[pixelIndex] = image.getDouble(x, y, k);
                        } else {
                            t1PixelValues[pixelIndex] = image.getDouble(x, y, k, t);
                        }
                        pixelIndex++;
                    }
                }
                
                if (dialog.isIncludeB1Map() == true) {
                    image = ViewUserInterface.getReference().getRegisteredImageByName(wList[b1ImageIndex]);
                    
                    pixelIndex = 0;
                    for (y=0; y<height; y++) {
                        for (x=0; x<width; x++) {
                            if(image.getNDims() < 4) {    
                                b1PixelValues[pixelIndex] = image.getDouble(x,y, k);
                            } else {
                                b1PixelValues[pixelIndex] = image.getDouble(x,y, k, t);
                            }
                            pixelIndex++;
                        }
                    }
                }
            
                // now that we have all the information, perform the calculate the initial estimates of B1, T2 and M0 pixel-wise
                pixelIndex = 0;
                
                for (y=0; y<height; y++) {
                    for (x=0; x<width; x++) {
                        
                        t1 = t1PixelValues[pixelIndex];
                        
                        if (t1 > 10.00) {
                            /*
                            if (t1 > 2500) {
                                b0Values[k-1][y][x] = 0.00;
                                m0Values[k-1][y][x] = maxM0;
                                t2Values[k-1][y][x] = maxT2;
                                r2Values[k-1][y][x] = 1.00/maxT2;
                                
                            }
                             
                            else {
                             */
                            
                            // scale up (or down) the flip angles based on the calculated B1 if required
                            if (dialog.isIncludeB1Map() == true) {
                                for (p=0; p<dialog.getNfa_phase0()+dialog.getNfa_phase180(); p++) { 
                                    scaledFA[p] = FA[p]*b1PixelValues[pixelIndex];
                                }
                            }
                            else {
                                for (p=0; p<dialog.getNfa_phase0()+dialog.getNfa_phase180(); p++) { 
                                    scaledFA[p] = FA[p];
                                }
                            }
                            
                            // calculate the sina and cosa matrices
                            for (p=0; p<dialog.getNfa_phase0()+dialog.getNfa_phase180(); p++) {
                                sina[p] = Math.sin(scaledFA[p]);
                                cosa[p] = Math.cos(scaledFA[p]);
                            }
                            
                            // grab the SSFP values for this pixel
                            for (p=0; p<dialog.getNfa_phase0(); p++) { 
                                ssfpSampleData[p] = ssfpPixelValues_phase0[p][pixelIndex];
                            }
                            for (p=0; p<dialog.getNfa_phase180(); p++) { 
                                ssfpSampleData[p+dialog.getNfa_phase0()] = ssfpPixelValues_phase180[p][pixelIndex];
                            }
                            
                            // begin with the downhill simplex
                        
                            // calculate an initial guess for M0, T2 and B0
                            if (dialog.isGeScanner()) {
                                initialGuess[0] = 10000.00; // initial guess for m0
                                initialGuess[1] = 100.00;   // initial guess for t2
                                initialGuess[2] = 500.0;    // initial guess for off-resonance (Hz)
                            }
                            else {
                                initialGuess[0] = 1000.00;
                                initialGuess[1] = 100.00;
                                initialGuess[2] = 500.0;
                            }
                            
                            threePDownHillSimplex(optimization, initialGuess, t1, 
                                    tr, ssfpSampleData, sina, cosa, phaseIncrements, dialog.getNfa_phase0()+dialog.getNfa_phase180());
                            
                            
                            m0 = optimization[0];
                            t2 = optimization[1];
                            b0 = optimization[2];
                            r2 = 0.00;
                            
                            if (m0 < 0.00) m0 = -1.00*m0;
                            if (t2 < 0.00) t2 = -1.00*t2;
                            if (b0 < 0.00) b0 = -1.00*b0;
                            
                            if (t2 > dialog.getMaxT2()) {
                                t2 = dialog.getMaxT2();
                            }
                            
                            // invert to r2
                            if (t2 != 0.00) {
                                r2 = 1.00/t2;
                            }
                            else {
                                r2 = 0.00;
                            }
                        
                            b0Values[k][y][x] = b0;
                            m0Values[k][y][x] = m0;
                            t2Values[k][y][x] = t2;
                            if (t1 > 0) r2Values[k][y][x] = 1.00/t2;
                            else r2Values[k][y][x] = 0.00;
                            //}
                        }
                        else {
                            b0Values[k][y][x] = 0.00;
                            m0Values[k][y][x] = 0.00;
                            t2Values[k][y][x] = 0.00;
                            r2Values[k][y][x] = 0.00;
                        }
                        
                        pixelIndex++;
                        
                    } // close the x (width) loop
                } // close the y (height) loop
            } // close the k (slice) loop


            // now, go back through and smooth the B0 field 
            for (k=0; k<nSlices; k++) {
                fireProgressStateChanged(prefix+"smoothing B0 field on slice: "+k+" of "+nSlices);
                fireProgressStateChanged(40+(int)(((float)k+1.0)/(float)nSlices*20.0));
                if(interrupted()) {
                    hardInterrupt = true;
                    return;
                }
                pixelIndex = 0;
                for (y=0; y<height; y++) {
                    for (x=0; x<width; x++) {
                        
                        if (y>2 && y<height-2 && x>2 && x<width-2) {
                            
                            smoothedB0 = 0.00;
                            for (p1=0; p1<5; p1++) {
                                for (p2=0; p2<5; p2++) smoothedB0 += b0Values[k][y-2+p2][x-2+p1]*Gaussian[p1][p2];
                            }
                            smoothedB0 = smoothedB0 / 34.00;
                            
                            b0Field[k][pixelIndex] = (float) smoothedB0;
                        }
                        else b0Field[k][pixelIndex] = (float) b0Values[k][y][x];
                        
                        pixelIndex ++;
                    }
                }
                
            }
        
            // now, go back through and calculate T2 and M0 again using the smoothed B0 value
            for (k=0; k<nSlices; k++) {
                fireProgressStateChanged(prefix+"recalculating values on slice: "+k+" of "+(nSlices-1));
                fireProgressStateChanged(60+(int)(((float)k+1.0)/(float)nSlices*15.0));
                if(interrupted()) {
                    hardInterrupt = true;
                    return;
                }
                // grab the ssfp pixel values from the phase = 0 data
                for (angle=0; angle<dialog.getNfa_phase0(); angle++) {
                    image = ViewUserInterface.getReference().getRegisteredImageByName(wList[ssfpImageIndex_phase0[angle]]);
                    
                    pixelIndex = 0;
                    for (y=0; y<height; y++) {
                        for (x=0; x<width; x++) {
                            if(image.getNDims() < 4) {
                                ssfpPixelValues_phase0[angle][pixelIndex] = image.getDouble(x, y, k);
                            } else {
                                ssfpPixelValues_phase0[angle][pixelIndex] = image.getDouble(x, y, k, t);
                            }
                            pixelIndex++;
                        }
                    }
                }
            
                // grab the ssfp pixel values from the phase = 180 data
                for (angle=0; angle<dialog.getNfa_phase180(); angle++) {
                    image = ViewUserInterface.getReference().getRegisteredImageByName(wList[ssfpImageIndex_phase180[angle]]);
                    
                    pixelIndex = 0;
                    for (y=0; y<height; y++) {
                        for (x=0; x<width; x++) {
                            if(image.getNDims() < 4) {
                                ssfpPixelValues_phase180[angle][pixelIndex] = image.getDouble(x, y, k);
                            } else {
                                ssfpPixelValues_phase180[angle][pixelIndex] = image.getDouble(x, y, k, t);
                            }
                            pixelIndex++;
                        }
                    }
                }
                
                // grab the T1 and B1 information
                image = ViewUserInterface.getReference().getRegisteredImageByName(wList[t1ImageIndex]);
                
                pixelIndex = 0;
                for (y=0; y<height; y++) {
                    for (x=0; x<width; x++) {
                        if(image.getNDims() < 4) {
                            t1PixelValues[pixelIndex] = image.getDouble(x, y, k);
                        } else {
                            t1PixelValues[pixelIndex] = image.getDouble(x, y, k, t);
                        }
                        pixelIndex++;
                    }
                }
                
                if (dialog.isIncludeB1Map() == true) {
                    image = ViewUserInterface.getReference().getRegisteredImageByName(wList[b1ImageIndex]);
                    
                    pixelIndex = 0;
                    for (y=0; y<height; y++) {
                        for (x=0; x<width; x++) {
                            if(image.getNDims() < 4) {
                                b1PixelValues[pixelIndex] = image.getDouble(x, y, k);
                            } else {
                                b1PixelValues[pixelIndex] = image.getDouble(x, y, k, t);
                            }
                            pixelIndex++;
                        }
                    }
                }
            
                pixelIndex = 0;
                for (y=0; y<height; y++) {
                    for (x=0; x<width; x++) {
                        
                        if (b0Field[k][pixelIndex] > 0.00) {
                
                        
                            b0 = b0Field[k][pixelIndex];
                            t1 = t1PixelValues[pixelIndex];
                            
                                    
                            // scale up (or down) the flip angles based on the calculated B1 if required
                            if (dialog.isIncludeB1Map() == true) {
                                for (p=0; p<dialog.getNfa_phase0()+dialog.getNfa_phase180(); p++) scaledFA[p] = FA[p]*b1PixelValues[pixelIndex];
                            }
                            else {
                                for (p=0; p<dialog.getNfa_phase0()+dialog.getNfa_phase180(); p++) scaledFA[p] = FA[p];
                            }
                            
                            // calculate the sina and cosa matrices
                            for (p=0; p<dialog.getNfa_phase0()+dialog.getNfa_phase180(); p++) {
                                sina[p] = Math.sin(scaledFA[p]);
                                cosa[p] = Math.cos(scaledFA[p]);
                            }
                            
                            // grab the SSFP values for this pixel
                            for (p=0; p<dialog.getNfa_phase0(); p++) ssfpSampleData[p] = ssfpPixelValues_phase0[p][pixelIndex];
                            for (p=0; p<dialog.getNfa_phase180(); p++) ssfpSampleData[p+dialog.getNfa_phase0()] = ssfpPixelValues_phase180[p][pixelIndex];
                            
                            
                            twoPInitialGuess[0] = m0Values[k][y][x];
                            twoPInitialGuess[1] = t2Values[k][y][x];
                            
                            twoPDownHillSimplex(twoPOptimization, twoPInitialGuess, b0, t1, tr, ssfpSampleData, sina, cosa, phaseIncrements, dialog.getNfa_phase0()+dialog.getNfa_phase180());
                            
                            m0 = twoPOptimization[0];
                            t2 = twoPOptimization[1];
                            
                            if (m0 < 0.00) m0 = -1.00*m0;
                            if (t2 < 0.00) t2 = -1.00*t2;
                            
                            if (t2 > dialog.getMaxT2()) t2 = dialog.getMaxT2();
                            
                            if (t2 > 0.00) r2 = 1.00/t2;
                            else r2 = 0.00;
                            
                            t2Field[k][pixelIndex] = (float) t2;
                            m0Field[k][pixelIndex] = (float) m0;
                            r2Field[k][pixelIndex] = (float) r2;
                        }
                        else {
                            t2Field[k][pixelIndex] = (float) 0.00;
                            m0Field[k][pixelIndex] = (float) 0.00;
                            r2Field[k][pixelIndex] = (float) 0.00;
                        }
                        
                        pixelIndex++;
                        
                    } // close x (width) loop
                } // close y (height) loop
            
                try {
                    // add data to the final stacks
                    int startVal = image.getSliceSize()*nSlices*t + image.getSliceSize()*k;
                    if (dialog.isCalculateT2()) { 
                        t2ResultStack.importData(startVal, t2Field[k], true);
                    }
                    if (dialog.isCalculateM0()) { 
                        m0ResultStack.importData(startVal, m0Field[k], true);
                    }
                    if (dialog.isInvertT2toR2()) { 
                        r2ResultStack.importData(startVal, r2Field[k], true);
                    }
                    if (dialog.isCalculateB0()) { 
                        b0ResultStack.importData(startVal, b0Field[k], true);
                    }
                } catch(IOException e) {
                    e.printStackTrace();
                    MipavUtil.displayError("Data could not be imported into result image");
                }// end the k (slice) loop 
            }
        }
        
        if (dialog.isCalculateT2()) {
            t2ResultWindow = new ViewJFrameImage(t2ResultStack);
            t2ResultWindow.setTitle("CalculatedT2Map_FM");
            t2ResultWindow.setVisible(true);
        } 
        
        if (dialog.isCalculateM0()) {
            m0ResultWindow = new ViewJFrameImage(m0ResultStack);
            m0ResultWindow.setTitle("CalculatedM0Map_FM");
            m0ResultWindow.setVisible(true);
        } 
        
        if (dialog.isInvertT2toR2()) {
            r2ResultWindow = new ViewJFrameImage(r2ResultStack);
            r2ResultWindow.setTitle("CalculatedR2Map_FM");
            r2ResultWindow.setVisible(true);
        } 
        
        if (dialog.isCalculateB0()) {
            b0ResultWindow = new ViewJFrameImage(b0ResultStack);
            b0ResultWindow.setTitle("CalculatedOffResonanceMap_FM");
            b0ResultWindow.setVisible(true);
        } 
    }
    
public void twoPDownHillSimplex(double[] optimization, double[] initialGuess, double b0, double t1, double tr, double[] ssfpSampleData, double[] sina, double[] cosa, double[] phaseIncrements, int N) {
        
        double RHO, CHI, PSI, SIGMA, maxError, usual_delta, zero_term_delta, residual;
        int best, worst, secondWorst;
        
        
        int NMAX, numParams, numVertices, iterations;
        
        double t2, m0;
        double rtol; 
        
        int i, j;
        
        // define algorithm-specific variables
        RHO = 1.00;
        CHI = 2.00;
        PSI = 0.50;
        SIGMA = 0.50;
        usual_delta = 0.05;
        zero_term_delta = .00025;
        
        maxError = 0.001;
        NMAX = 15000;
        
        numParams = 2;
        numVertices = numParams+1;
        
        // initialize the twoPSimplex
        twoPSimplexLineValues[0] = initialGuess[0]; 
        twoPSimplexLineValues[1] = initialGuess[1];
        residual = calculateTwoPResiduals(twoPSimplexLineValues, b0, t1, tr, ssfpSampleData, sina, cosa, phaseIncrements, N);
        
        for (i=0; i<numParams; i++) twoPSimplex[0][i] = twoPSimplexLineValues[i];
        twoPSimplex[0][numParams] = residual;
        
        for (i=0; i<numParams; i++) {
            twoPSimplexLineValues[0] = initialGuess[0];
            twoPSimplexLineValues[1] = initialGuess[1];
            
            if (twoPSimplexLineValues[i] != 0.00) twoPSimplexLineValues[i] = (1.00+usual_delta)*twoPSimplexLineValues[i];
            else twoPSimplexLineValues[i] = zero_term_delta;
            residual = calculateTwoPResiduals(twoPSimplexLineValues, b0, t1, tr, ssfpSampleData, sina, cosa, phaseIncrements, N);
            
            twoPSimplex[i+1][0] = twoPSimplexLineValues[0];
            twoPSimplex[i+1][1] = twoPSimplexLineValues[1];
            twoPSimplex[i+1][2] = residual;
        }
        
        // now calculate the best, worst and second worst vertices of the twoPSimplex
        for (i=0; i<numVertices; i++) twoPSimplexResiduals[i] = twoPSimplex[i][numParams];
        calculateBestToWorst(twoPSimplexResiduals, bestToWorst, numVertices);
        best = bestToWorst[0];
        worst = bestToWorst[2];
        secondWorst = bestToWorst[1];
        
        // define a fractional range from the best to worst twoPSimplex vertex
        rtol = twoPSimplex[best][numParams] - twoPSimplex[worst][numParams];
        if (rtol < 0.00) rtol = -1.00*rtol;
        
        // now, get on with the main algorithm
        iterations = 0;
        while (rtol > maxError && iterations < NMAX) {
            iterations++;
            
            // calculate the centre of the twoPSimplex, ignoring the worst point
            for (i=0; i<numParams; i++) twoPSimplexCentre[i] = 0.00;
            for (i=0; i<numVertices; i++) {
                if (i != bestToWorst[numParams-1]) {
                    for (j=0; j<numParams; j++) twoPSimplexCentre[j] += twoPSimplex[i][j];
                }
            }
            for (i=0; i<numParams; i++) twoPSimplexCentre[i] = twoPSimplexCentre[i]/numParams;
            
            // reflect the twoPSimplex through the face of the high point
            //for (i=0; i<numVertices; i++) twoPReflection[i] = 0.00;
            for (i=0; i<numParams; i++) twoPReflection[i] = (1.00+RHO)*twoPSimplexCentre[i] - RHO*twoPSimplex[worst][i];
            twoPReflection[numParams] = calculateTwoPResiduals(twoPReflection, b0, t1, tr, ssfpSampleData, sina, cosa, phaseIncrements, N);
            
            
            // compare the twoPReflection vertex with the best vertex in the exisiting twoPSimplex
            if (twoPReflection[numParams] < twoPSimplex[best][numParams]) {
                // if the twoPReflection was better, try an twoPExpansion in this direction and see how that is
                //for (i=0; i<numVertices; i++) twoPExpansion[i] = 0.00;
                for (i=0; i<numParams; i++) twoPExpansion[i] = (1.00+RHO*CHI)*twoPSimplexCentre[i] - RHO*CHI*twoPSimplex[worst][i];
                twoPExpansion[numParams] = calculateTwoPResiduals(twoPExpansion, b0, t1, tr, ssfpSampleData, sina, cosa, phaseIncrements, N);
                
                if (twoPExpansion[numParams] < twoPReflection[numParams]) {
                    for (i=0; i<numVertices; i++) twoPSimplex[worst][i] = twoPExpansion[i];
                }
                else {
                    for (i=0; i<numVertices; i++) twoPSimplex[worst][i] = twoPReflection[i];
                }
            }
            else {
                if (twoPReflection[numParams] < twoPSimplex[secondWorst][numParams]) {
                    for (i=0; i<numVertices; i++) twoPSimplex[worst][i] = twoPReflection[i];
                }
                else {
                    if (twoPReflection[numParams] < twoPSimplex[worst][numParams]) {
                        // perform an outside twoPContraction
                        //for (i=0; i<numVertices; i++) twoPContraction[i] = 0.00;
                        for (i=0; i<numParams; i++) twoPContraction[i] = (1.00+PSI*RHO)*twoPSimplexCentre[i] - RHO*PSI*twoPSimplex[worst][i];
                        twoPContraction[numParams] = calculateTwoPResiduals(twoPContraction, b0, t1, tr, ssfpSampleData, sina, cosa, phaseIncrements, N);
                        
                        if (twoPContraction[numParams] <= twoPReflection[numParams]) {
                            for (i=0; i<numVertices; i++) twoPSimplex[worst][i] = twoPContraction[i];
                        }
                        else {
                            // perform a twoPShrink of all vertices except the best
                            for (j=0; j<numVertices; j++) {
                                if (j != best) {
                                    //for (i=0; i<numVertices; i++) twoPShrink[i] = 0.00;
                                    for (i=0; i<numParams; i++) twoPShrink[i] = twoPSimplex[best][i] + SIGMA*(twoPSimplex[j][i]-twoPSimplex[best][i]);
                                    twoPShrink[numParams] = calculateTwoPResiduals(twoPShrink, b0, t1, tr, ssfpSampleData, sina, cosa, phaseIncrements, N);
                                    for (i=0; i<numVertices; i++) twoPSimplex[j][i] = twoPShrink[i];
                                }
                            }
                        }
                    }
                    else {
                        // perform an inside twoPContraction
                        //for (i=0; i<numVertices; i++) twoPContraction[i] = 0.00;
                        for (i=0; i<numParams; i++) twoPContraction[i] = (1.00-PSI)*twoPSimplexCentre[i] + PSI*twoPSimplex[worst][i];
                        twoPContraction[numParams] = calculateTwoPResiduals(twoPContraction, b0, t1, tr, ssfpSampleData, sina, cosa, phaseIncrements, N);
                        
                        if (twoPContraction[numParams] < twoPSimplex[worst][numParams]) {
                            for (i=0; i<numVertices; i++) twoPSimplex[worst][i] = twoPContraction[i];
                        }
                        else {
                            // perform a twoPShrink of all vertices except the best
                            for (j=0; j<numVertices; j++) {
                                if (j != best) {
                                    //for (i=0; i<numVertices; i++) twoPShrink[i] = 0.00;
                                    for (i=0; i<numParams; i++) twoPShrink[i] = twoPSimplex[best][i] + SIGMA*(twoPSimplex[j][i]-twoPSimplex[best][i]);
                                    twoPShrink[numParams] = calculateTwoPResiduals(twoPShrink, b0, t1, tr, ssfpSampleData, sina, cosa, phaseIncrements, N);
                                    for (i=0; i<numVertices; i++) twoPSimplex[j][i] = twoPShrink[i];
                                }
                            }
                        }
                    }
                }
                
            }
            
            // re-evaluate the twoPSimplex vertices from best to worst
            for (i=0; i<numVertices; i++) twoPSimplexResiduals[i] = twoPSimplex[i][numVertices-1];
            calculateBestToWorst(twoPSimplexResiduals, bestToWorst, numVertices);
            best = bestToWorst[0];
            worst = bestToWorst[2];
            secondWorst = bestToWorst[1];
            
            rtol = twoPSimplex[best][numParams] - twoPSimplex[worst][numParams];
            if (rtol < 0.00) rtol = -1.00*rtol;
            
        } // end of main algorithm loop
        
        m0 = twoPSimplex[best][0];
        t2 = twoPSimplex[best][1];
        
        if (m0 < 0.00) m0 = -1.00*m0;
        if (t2 < 0.00) t2 = -1.00*t2;
        
        optimization[0] = m0;
        optimization[1] = t2;
        
        return;
    }
    
    public void threePDownHillSimplex(double[] optimization, double[] initialGuess, double t1, double tr, double[] ssfpSampleData, double[] sina, double[] cosa, double[] phaseIncrements, int N) {
        
        double RHO, CHI, PSI, SIGMA, maxError, usual_delta, zero_term_delta, residual;
        int best, worst, secondWorst;
        
        
        int NMAX, numParams, numVertices, iterations;
        
        double t2, m0, b0;
        double rtol; 
        
        int i, j;
        
        // define algorithm-specific variables
        RHO = 1.00;
        CHI = 2.00;
        PSI = 0.50;
        SIGMA = 0.50;
        usual_delta = 0.05;
        zero_term_delta = .00025;
        
        maxError = 0.001;
        NMAX = 15000;
        
        numParams = 3;
        numVertices = numParams+1;
        
        // initialize the simplex
        simplexLineValues[0] = initialGuess[0]; 
        simplexLineValues[1] = initialGuess[1];
        simplexLineValues[2] = initialGuess[2]; 
        residual = calculateResiduals(simplexLineValues, t1, tr, ssfpSampleData, sina, cosa, phaseIncrements, N);
        
        for (i=0; i<numParams; i++) simplex[0][i] = simplexLineValues[i];
        simplex[0][numParams] = residual;
        
        for (i=0; i<numParams; i++) {
            simplexLineValues[0] = initialGuess[0];
            simplexLineValues[1] = initialGuess[1];
            simplexLineValues[2] = initialGuess[2];
            
            if (simplexLineValues[i] != 0.00) simplexLineValues[i] = (1.00+usual_delta)*simplexLineValues[i];
            else simplexLineValues[i] = zero_term_delta;
            residual = calculateResiduals(simplexLineValues, t1, tr, ssfpSampleData, sina, cosa, phaseIncrements, N);
            
            simplex[i+1][0] = simplexLineValues[0];
            simplex[i+1][1] = simplexLineValues[1];
            simplex[i+1][2] = simplexLineValues[2];
            simplex[i+1][3] = residual;
        }
        
        // now calculate the best, worst and second worst vertices of the simplex
        for (i=0; i<numVertices; i++) simplexResiduals[i] = simplex[i][numParams];
        calculateBestToWorst(simplexResiduals, bestToWorst, numVertices);
        best = bestToWorst[0];
        worst = bestToWorst[2];
        secondWorst = bestToWorst[1];
        
        // define a fractional range from the best to worst simplex vertex
        rtol = simplex[best][numParams] - simplex[worst][numParams];
        if (rtol < 0.00) rtol = -1.00*rtol;
        
        // now, get on with the main algorithm
        iterations = 0;
        while (rtol > maxError && iterations < NMAX) {
            iterations++;
            
            // calculate the centre of the simplex, ignoring the worst point
            for (i=0; i<numParams; i++) simplexCentre[i] = 0.00;
            for (i=0; i<numVertices; i++) {
                if (i != bestToWorst[numParams-1]) {
                    for (j=0; j<numParams; j++) simplexCentre[j] += simplex[i][j];
                }
            }
            for (i=0; i<numParams; i++) simplexCentre[i] = simplexCentre[i]/numParams;
            
            // reflect the simplex through the face of the high point
            //for (i=0; i<numVertices; i++) reflection[i] = 0.00;
            for (i=0; i<numParams; i++) reflection[i] = (1.00+RHO)*simplexCentre[i] - RHO*simplex[worst][i];
            reflection[numParams] = calculateResiduals(reflection, t1, tr, ssfpSampleData, sina, cosa, phaseIncrements, N);
            
            
            // compare the reflection vertex with the best vertex in the exisiting simplex
            if (reflection[numParams] < simplex[best][numParams]) {
                // if the reflection was better, try an expansion in this direction and see how that is
                //for (i=0; i<numVertices; i++) expansion[i] = 0.00;
                for (i=0; i<numParams; i++) expansion[i] = (1.00+RHO*CHI)*simplexCentre[i] - RHO*CHI*simplex[worst][i];
                expansion[numParams] = calculateResiduals(expansion, t1, tr, ssfpSampleData, sina, cosa, phaseIncrements, N);
                
                if (expansion[numParams] < reflection[numParams]) {
                    for (i=0; i<numVertices; i++) simplex[worst][i] = expansion[i];
                }
                else {
                    for (i=0; i<numVertices; i++) simplex[worst][i] = reflection[i];
                }
            }
            else {
                if (reflection[numParams] < simplex[secondWorst][numParams]) {
                    for (i=0; i<numVertices; i++) simplex[worst][i] = reflection[i];
                }
                else {
                    if (reflection[numParams] < simplex[worst][numParams]) {
                        // perform an outside contraction
                        //for (i=0; i<numVertices; i++) contraction[i] = 0.00;
                        for (i=0; i<numParams; i++) contraction[i] = (1.00+PSI*RHO)*simplexCentre[i] - RHO*PSI*simplex[worst][i];
                        contraction[numParams] = calculateResiduals(contraction, t1, tr, ssfpSampleData, sina, cosa, phaseIncrements, N);
                        
                        if (contraction[numParams] <= reflection[numParams]) {
                            for (i=0; i<numVertices; i++) simplex[worst][i] = contraction[i];
                        }
                        else {
                            // perform a shrink of all vertices except the best
                            for (j=0; j<numVertices; j++) {
                                if (j != best) {
                                    //for (i=0; i<numVertices; i++) shrink[i] = 0.00;
                                    for (i=0; i<numParams; i++) shrink[i] = simplex[best][i] + SIGMA*(simplex[j][i]-simplex[best][i]);
                                    shrink[numParams] = calculateResiduals(shrink, t1, tr, ssfpSampleData, sina, cosa, phaseIncrements, N);
                                    for (i=0; i<numVertices; i++) simplex[j][i] = shrink[i];
                                }
                            }
                        }
                    }
                    else {
                        // perform an inside contraction
                        //for (i=0; i<numVertices; i++) contraction[i] = 0.00;
                        for (i=0; i<numParams; i++) contraction[i] = (1.00-PSI)*simplexCentre[i] + PSI*simplex[worst][i];
                        contraction[numParams] = calculateResiduals(contraction, t1, tr, ssfpSampleData, sina, cosa, phaseIncrements, N);
                        
                        if (contraction[numParams] < simplex[worst][numParams]) {
                            for (i=0; i<numVertices; i++) simplex[worst][i] = contraction[i];
                        }
                        else {
                            // perform a shrink of all vertices except the best
                            for (j=0; j<numVertices; j++) {
                                if (j != best) {
                                    //for (i=0; i<numVertices; i++) shrink[i] = 0.00;
                                    for (i=0; i<numParams; i++) shrink[i] = simplex[best][i] + SIGMA*(simplex[j][i]-simplex[best][i]);
                                    shrink[numParams] = calculateResiduals(shrink, t1, tr, ssfpSampleData, sina, cosa, phaseIncrements, N);
                                    for (i=0; i<numVertices; i++) simplex[j][i] = shrink[i];
                                }
                            }
                        }
                    }
                }
                
            }
            
            // re-evaluate the simplex vertices from best to worst
            for (i=0; i<numVertices; i++) simplexResiduals[i] = simplex[i][numVertices-1];
            calculateBestToWorst(simplexResiduals, bestToWorst, numVertices);
            best = bestToWorst[0];
            worst = bestToWorst[2];
            secondWorst = bestToWorst[1];
            
            rtol = simplex[best][numParams] - simplex[worst][numParams];
            if (rtol < 0.00) rtol = -1.00*rtol;
            
        } // end of main algorithm loop
        
        m0 = simplex[best][0];
        t2 = simplex[best][1];
        b0 = simplex[best][2];
        
        if (m0 < 0.00) m0 = -1.00*m0;
        if (t2 < 0.00) t2 = -1.00*t2;
        if (b0 < 0.00) b0 = -1.00*b0;
        
        optimization[0] = m0;
        optimization[1] = t2;
        optimization[2] = b0;
        
        return;
    }
    
    
    public double calculateTwoPResiduals(double[] simplexLineValues, double b0, double t1, double tr, double[] Signal, double[] sina, double[] cosa, double[] phaseIncrements, int N) {
        
        double m0, e1, e2, phasePrecession, pi, mx, my, guessSignal, residualValue;
        int i;
        
        pi = 3.14159265;
        m0 = simplexLineValues[0];
        e1 = Math.exp(-tr/t1);
        e2 = Math.exp(-tr/simplexLineValues[1]);
        
        residualValue = 0.00;
        for (i=0; i<N; i++) {
            phasePrecession = phaseIncrements[i] + 2*pi*((tr/1000.00)*b0);
            
            mx = m0*(1.00-e1)*e2*sina[i]*(Math.cos(phasePrecession)-e2)/( (1.00-e1*cosa[i])*(1.00-e2*Math.cos(phasePrecession))-e2*(e1-cosa[i])*(e2-Math.cos(phasePrecession)) );
            my = m0*(1.00-e1)*e2*sina[i]*Math.sin(phasePrecession)/( (1.00-e1*cosa[i])*(1.00-e2*Math.cos(phasePrecession))-e2*(e1-cosa[i])*(e2-Math.cos(phasePrecession)) );
            
            guessSignal = Math.sqrt( Math.pow(mx,2.00) + Math.pow(my,2.00) );
            
            residualValue += Math.pow( (Signal[i]-guessSignal),2.00 );
        }
        
        return residualValue;
    }
    
    public double calculateResiduals(double[] simplexLineValues, double t1, double tr, double[] Signal, double[] sina, double[] cosa, double[] phaseIncrements, int N) {
        
        double m0, e1, e2, phasePrecession, pi, mx, my, guessSignal, residualValue;
        int i;
        
        pi = 3.14159265;
        m0 = simplexLineValues[0];
        e1 = Math.exp(-tr/t1);
        e2 = Math.exp(-tr/simplexLineValues[1]);
        
        residualValue = 0.00;
        for (i=0; i<N; i++) {
            phasePrecession = phaseIncrements[i] + 2*pi*((tr/1000.00)*simplexLineValues[2]);
            
            mx = m0*(1.00-e1)*e2*sina[i]*(Math.cos(phasePrecession)-e2)/( (1.00-e1*cosa[i])*(1.00-e2*Math.cos(phasePrecession))-e2*(e1-cosa[i])*(e2-Math.cos(phasePrecession)) );
            my = m0*(1.00-e1)*e2*sina[i]*Math.sin(phasePrecession)/( (1.00-e1*cosa[i])*(1.00-e2*Math.cos(phasePrecession))-e2*(e1-cosa[i])*(e2-Math.cos(phasePrecession)) );
            
            guessSignal = Math.sqrt( Math.pow(mx,2.00) + Math.pow(my,2.00) );
            
            residualValue += Math.pow( (Signal[i]-guessSignal),2.00 );
        }
        
        return residualValue;
    }
    
    public double calculate2PResiduals(double[] simplexLineValues, double b0, double t1, double tr, double[] Signal, double[] sina, double[] cosa, double[] phaseIncrements, int N) {
        
        double m0, e1, e2, phasePrecession, pi, mx, my, guessSignal, residualValue;
        int i;
        
        pi = 3.14159265;
        m0 = simplexLineValues[0];
        e1 = Math.exp(-tr/t1);
        e2 = Math.exp(-tr/simplexLineValues[1]);
        
        residualValue = 0.00;
        for (i=0; i<N; i++) {
            phasePrecession = phaseIncrements[i] + 2*pi*((tr/1000.00)*b0);
            
            mx = m0*(1.00-e1)*e2*sina[i]*(Math.cos(phasePrecession)-e2)/( (1.00-e1*cosa[i])*(1.00-e2*Math.cos(phasePrecession))-e2*(e1-cosa[i])*(e2-Math.cos(phasePrecession)) );
            my = m0*(1.00-e1)*e2*sina[i]*Math.sin(phasePrecession)/( (1.00-e1*cosa[i])*(1.00-e2*Math.cos(phasePrecession))-e2*(e1-cosa[i])*(e2-Math.cos(phasePrecession)) );
            
            guessSignal = Math.sqrt( Math.pow(mx,2.00) + Math.pow(my,2.00) );
            
            residualValue += Math.pow( (Signal[i]-guessSignal),2.00 );
        }
        
        return residualValue;
    }
    
    public void calculateBestToWorst(double[] simplexResiduals, int[] bestToWorst, int numVertices) {
        
        int i, best, worst, secondWorst;
        
        best = 0;
        worst = 0;
        secondWorst = 0;
        
        for (i=0; i<numVertices; i++) {
            if (simplexResiduals[i] < simplexResiduals[best]) best = i;
            if (simplexResiduals[i] > simplexResiduals[worst]) worst = i;
        }
        
        secondWorst = best;
        for (i=0; i<numVertices; i++) {
            if (i != worst) {
                if (simplexResiduals[i] > simplexResiduals[secondWorst]) secondWorst = i;
            }
        }
        bestToWorst[0] = best;
        bestToWorst[1] = secondWorst;
        bestToWorst[2] = worst;
        
        return;
    }
    
    /**
     * This method gives <code>clo</code> most of the image attributes of <code>orig</code> besides
     * data type to minimize rounding errors.
     * 
     * @param orig Original image
     * @param clo Near clone image
     */
    public ModelImage nearCloneImage(ModelImage orig, ModelImage clo) {
        FileInfoBase[] oArr = orig.getFileInfo();
        FileInfoBase[] cArr = clo.getFileInfo();
        if(cArr.length != oArr.length) {
            MipavUtil.displayError("Images are not same length");
            return clo;
        }
        for(int i=0; i<cArr.length; i++) {
            if(cArr == null) {
                return clo;
            }
            cArr[i].setOffset(oArr[i].getOffset());
            cArr[i].setEndianess(oArr[i].getEndianess());
            cArr[i].setResolutions(oArr[i].getResolutions().clone());
            cArr[i].setUnitsOfMeasure(oArr[i].getUnitsOfMeasure().clone());
            cArr[i].setOrigin(oArr[i].getOrigin().clone());
            cArr[i].setImageOrientation(cArr[i].getImageOrientation());
            cArr[i].setAxisOrientation(oArr[i].getAxisOrientation().clone());
            cArr[i].setDataType(ModelImage.DOUBLE);
        }
        
        return clo;
    }
    
    public void reduceB0Field(double[][] b0Field, double resonancePeriod, int width, int height) {
        int i,j;
        double fraction, offResonanceMod;
        int repetitionCycle;
        
        for (i=0; i<width; i++) {
            for (j=0; j<height; j++) {
                
                repetitionCycle = 0;
                
                fraction = b0Field[i][j]/resonancePeriod;
                if (fraction >= 1) repetitionCycle = (int) (Math.floor(fraction));
                if (fraction < 1 && fraction >= 0.5) repetitionCycle = 1;
                if (fraction < 0.5) repetitionCycle = 0;
                
                offResonanceMod = b0Field[i][j] - (repetitionCycle*resonancePeriod);
                if (offResonanceMod < 0.00) offResonanceMod = -1.00*offResonanceMod;
                
                b0Field[i][j] = offResonanceMod;
                
                
            }
        }
        
        return;
    }
    
    
    public void smoothField(double[][] field, float[][] fieldValues, int width, int height, int k) {
        int x, y, p1, p2;
        int pixelIndex;
        double smoothedValue;
        
        pixelIndex = 0;
        
        for (x=0; x<width; x++) {
            for (y=0; y<height; y++) {
                
                if (y>2 && y<height-2 && x>2 && x<width-2) {
                    smoothedValue = 0.00;
                    for (p1=0; p1<5; p1++) {
                        for (p2=0; p2<5; p2++) smoothedValue += field[x-2+p1][y-2+p2]*Gaussian[p1][p2];
                    }
                    smoothedValue = smoothedValue / 34.00;
                    
                    fieldValues[k-1][pixelIndex] = (float) smoothedValue;
                }
                else fieldValues[k-1][pixelIndex] = (float) 0.00;
                
                pixelIndex++;
                
                
            } // close y loop
        } // close x loop
        
        
        return;
    }
    
    public void smoothFieldB(double[][] field, int width, int height) {
        int x, y, p1, p2;
        double smoothedValue;
        double[][] smoothedField;
        
        smoothedField = new double[width][height];
        for (y=0; y<height; y++) {
            for (x=0; x<width; x++) {
                smoothedField[x][y] = 0.00;
            }
        }
        
        
        for (y=2; y<height-2; y++) {
            for (x=2; x<width-2; x++) {
                
                smoothedValue = 0.00;
                for (p1=0; p1<5; p1++) {
                    for (p2=0; p2<5; p2++) {
                    	smoothedValue += field[x-2+p1][y-2+p2]; //Gaussian[p1][p2]*;
                    }
                }
                smoothedValue = smoothedValue / 34.00;
                smoothedField[x][y] = smoothedValue;
                
            } // close y loop
        } // close x loop
        
         
        
        for (y=0; y<height; y++) {
            for (x=0; x<width; x++) {
                field[x][y] = smoothedField[x][y];
            }
        }
        
        return;
    }
        
    
    public void resetSliceToZero(double[][] field, int width, int height) {
        int x, y;
        
        for (y=0; y<height; y++) {
            for (x=0; x<width; x++) {
                
                field[x][y] = 0.00;
                
                
            } // close y loop
        } // close x loop
        
        
        return;
    }
    
    public void swapMatrixForVector(double[][] fieldField, float[][] fieldValues, int k, int width, int height) {
        int x, y, pixelIndex;
        
        pixelIndex = 0;
        
        for (x=0; x<width; x++) {
            for (y=0; y<height; y++) {
                fieldValues[k-1][pixelIndex] = (float) fieldField[x][y];
                pixelIndex++;
            }
        }
        
        return;
    }
    
    public void swapMatrixForVectorB(double[][] fieldField, float[][] fieldValues, int k, int width, int height) {
        int x, y, pixelIndex;
        
        pixelIndex = 0;
        
        for (y=0; y<height; y++) {
            for (x=0; x<width; x++) {
                fieldValues[k-1][pixelIndex] = (float) fieldField[x][y];
                pixelIndex++;
            }
        }
        
        return;
    }

	public ModelImage getT2ResultStack() {
		return t2ResultStack;
	}

	public ModelImage getM0ResultStack() {
		return m0ResultStack;
	}

	public ModelImage getR2ResultStack() {
		return r2ResultStack;
	}

	public ModelImage getB0ResultStack() {
		return b0ResultStack;
	}

	@Override
	protected void computeProcessors() {
		// TODO Auto-generated method stub
		
	}

	@Override
	protected void displayImages() {
		// TODO Auto-generated method stub
		
	}
}


