package gov.nih.mipav.model.algorithms;

import gov.nih.mipav.model.file.FileInfoBase;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.ViewJFrameImage;
import gov.nih.mipav.view.ViewUserInterface;
import gov.nih.mipav.view.dialogs.JDialogDespotT2;

import java.io.IOException;

public class AlgorithmDespotT2 extends AlgorithmBase {

    private double[] despotFA_phase0;
    private double[] despotFA_phase180;
    
    private int[] ssfpImageIndex_phase0;
    private int[] ssfpImageIndex_phase180;
    private int t1ImageIndex;
    private int b1ImageIndex;
    
    private double[] simplexLineValues, simplexResiduals, simplexCentre, reflection, expansion, contraction, shrink;
    private double[][] simplex;
    private double[] twoPSimplexLineValues, twoPSimplexResiduals, twoPSimplexCentre, twoPReflection, twoPExpansion, twoPContraction, twoPShrink;
    private double[][] twoPSimplex;
    private int[] bestToWorst;
    
    private boolean hardInterrupt = false;
    private ModelImage t2ResultStack;
    private ModelImage moResultStack;
    private ModelImage r2ResultStack;
    private ModelImage boResultStack;
    
    private String[] wList;
    
    
    
    public AlgorithmDespotT2(double[] despotFAPhase0,
            double[] despotFAPhase180, int[] ssfpImageIndexPhase0,
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
        despotFA_phase0 = despotFAPhase0;
        despotFA_phase180 = despotFAPhase180;
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

    public AlgorithmDespotT2(ModelImage destImage, ModelImage srcImage,
            double[] despotFAPhase0, double[] despotFAPhase180,
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
        despotFA_phase0 = despotFAPhase0;
        despotFA_phase180 = despotFAPhase180;
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
        if(t2ResultStack != null) {
            t2ResultStack.disposeLocal();
        }
        
        if(moResultStack != null) {
            moResultStack.disposeLocal();
        }

        if(r2ResultStack != null) {
            r2ResultStack.disposeLocal();
        }

        if(boResultStack != null) {
            boResultStack.disposeLocal();
        }
    }
    
    public void runAlgorithm() {
        if (JDialogDespotT2.performFullModelling == true || JDialogDespotT2.performApproxModelling == true) {
            if (JDialogDespotT2.performFullModelling == true) calculateT2withFullModelling();
            else calculateT2withApproximateModelling();
        }
        else {
            if (JDialogDespotT2.performConventionalWith0Phase == true) calculateT2with0Phase();
            else calculateT2with180Phase();
        }
    }
    
    public void calculateT2with0Phase() {
        ModelImage image;
        float[] ctable;
        
        
        double[] fa_phase0;
        double[] scaledFA_phase0;
        
        double[][] ssfpPixelValues_phase0;
        double[] t1PixelValues, b1PixelValues;
        double[] phase0Data;
        
        float[][] t2Values, moValues, r2Values;
        
        double a, d, e2;
        double sumX, sumY, sumXY, sumXX, slope, denominator, intercept, lnslope, t1, t2, e1, mo, r2;
        double x1, x2, y1, y2;
        double[] possibleT2s, possibleMos;
        float noiseSum, threshold;
        
        int width, height, nSlices;
        int x,y,i,j,k,angle, p, ti, p1, p2, pixelIndex, noiseIndex;
        
        image = ViewUserInterface.getReference().getRegisteredImageByName(wList[ssfpImageIndex_phase0[0]]);
        width = image.getExtents()[0];
        height = image.getExtents()[1];
        if(image.getNDims() > 2) {
            nSlices = image.getExtents()[2];
        } else {
            nSlices = 1;
        }
        
        ssfpPixelValues_phase0 = new double[JDialogDespotT2.Nfa_phase0][width*height];
        t1PixelValues = new double[width*height];
        if (JDialogDespotT2.includeB1Map) b1PixelValues = new double[width*height];
        else b1PixelValues = new double[1];
        
        if (JDialogDespotT2.calculateT2) t2Values = new float[nSlices][width*height];
        else t2Values = new float[1][1];
        if (JDialogDespotT2.calculateMo) moValues = new float[nSlices][width*height];
        else moValues = new float[1][1];
        if (JDialogDespotT2.invertT2toR2) r2Values = new float[nSlices][width*height];
        else r2Values = new float[1][1];
        
        t2ResultStack = new ModelImage(ModelImage.DOUBLE, image.getExtents(), "t2 Results");
        moResultStack = new ModelImage(ModelImage.DOUBLE, image.getExtents(), "mo Results");
        r2ResultStack = new ModelImage(ModelImage.DOUBLE, image.getExtents(), "r2 Results");
        
        t2ResultStack = nearCloneImage(image, t2ResultStack);
        moResultStack = nearCloneImage(image, moResultStack);
        r2ResultStack = nearCloneImage(image, r2ResultStack);
        
        fa_phase0 = new double[JDialogDespotT2.Nfa_phase0];
        scaledFA_phase0 = new double[JDialogDespotT2.Nfa_phase0];
        for (angle=0; angle<JDialogDespotT2.Nfa_phase0; angle++) {
            fa_phase0[angle] = Math.toRadians(despotFA_phase0[angle]);
        }
        
        phase0Data = new double[JDialogDespotT2.Nfa_phase0];
        
        // Actually perform the T2 Calculations
        for (k=0; k<nSlices; k++) {
            fireProgressStateChanged("calculating T2 values on slice: "+k+" of "+nSlices);
            fireProgressStateChanged(0+(int)(((float)k+1.0)/(float)nSlices*90.0));
            if(interrupted()) {
                hardInterrupt = true;
                return;
            }
            // grab the ssfp pixel values from the phase = 0 data
            for (angle=0; angle<JDialogDespotT2.Nfa_phase0; angle++) {
                image = ViewUserInterface.getReference().getRegisteredImageByName(wList[ssfpImageIndex_phase0[angle]]);
                pixelIndex = 0;
                for (y=0; y<height; y++) {
                    for (x=0; x<width; x++) {
                        ssfpPixelValues_phase0[angle][pixelIndex] = image.getDouble(x, y, k);
                        pixelIndex++;
                    }
                }
            }
            
            // grab the T1 and B1 information
            image = ViewUserInterface.getReference().getRegisteredImageByName(wList[t1ImageIndex]);
            
            pixelIndex = 0;
            for (y=0; y<height; y++) {
                for (x=0; x<width; x++) {
                    t1PixelValues[pixelIndex] = image.getDouble(x, y, k);
                    pixelIndex++;
                }
            }
            
            if (JDialogDespotT2.includeB1Map == true) {
                image = ViewUserInterface.getReference().getRegisteredImageByName(wList[b1ImageIndex]);
                
                pixelIndex = 0;
                for (y=0; y<height; y++) {
                    for (x=0; x<width; x++) {
                        b1PixelValues[pixelIndex] = image.getDouble(x, y, k);
                        pixelIndex++;
                    }
                }
            }
            
            // now that we have all the information, perform the calculate pixel-wise
            pixelIndex = 0;
            for (x=0; x<width; x++) {
                for (y=0; y<height; y++) {
                    
                    if (t1PixelValues[pixelIndex] > 0.00) {
                        
                        e1 = Math.exp(-JDialogDespotT2.despotTR/t1PixelValues[pixelIndex]);
                        
                        // scale up (or down) the flip angles based on the calculated B1 if required
                        if (JDialogDespotT2.includeB1Map == true) {
                            for (p=0; p<JDialogDespotT2.Nfa_phase0; p++) scaledFA_phase0[p] = fa_phase0[p]*b1PixelValues[pixelIndex];
                        }
                        else {
                            for (p=0; p<JDialogDespotT2.Nfa_phase0; p++) scaledFA_phase0[p] = fa_phase0[p];
                        }
                        
                        // grab the SSFP values for this pixel
                        for (p=0; p<JDialogDespotT2.Nfa_phase0; p++) phase0Data[p] = ssfpPixelValues_phase0[p][pixelIndex];
                        
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
                        
                        for (p=0; p<JDialogDespotT2.Nfa_phase0; p++) {
                            sumX += phase0Data[p]/Math.tan(scaledFA_phase0[p]);
                            sumY += phase0Data[p]/Math.sin(scaledFA_phase0[p]);
                            sumXY += (phase0Data[p]/Math.tan(scaledFA_phase0[p]))*(phase0Data[p]/Math.sin(scaledFA_phase0[p]));
                            sumXX += (phase0Data[p]/Math.tan(scaledFA_phase0[p]))*(phase0Data[p]/Math.tan(scaledFA_phase0[p]));
                        }
                        
                        d = (JDialogDespotT2.Nfa_phase0*sumXX) - sumX*sumX;
                        a = (JDialogDespotT2.Nfa_phase0*sumXY) - (sumX*sumY);
                        
                        if (d != 0) {
                            slope = a/d;
                            denominator = (e1-slope)/(1.00-slope*e1);
                            intercept = (sumY-slope*sumX)/JDialogDespotT2.Nfa_phase0;
                            
                            if (denominator > 0.00 && denominator < 1.00) {
                                t2 = -JDialogDespotT2.despotTR/Math.log(denominator);
                                e2 = Math.exp(-JDialogDespotT2.despotTR/t2);
                                mo = intercept*(1.00-e1*e2)/(1.00-e1);
                            }
                            else {
                                mo = JDialogDespotT2.maxMo;
                                t2 = JDialogDespotT2.maxT2;
                            }
                        }
                        else {
                            mo = JDialogDespotT2.maxMo;
                            t2 = JDialogDespotT2.maxT2;
                        }
                        
                        
                        if (t2 < 0.00 || t2 > JDialogDespotT2.maxT2) {
                            t2 = JDialogDespotT2.maxT2;
                        }
                        if (mo < 0.00 || mo > JDialogDespotT2.maxMo) {
                            mo = JDialogDespotT2.maxMo;
                        }
                        
                        // invert to r2
                        if (t2 != 0.00) {
                            r2 = 1.00/t2;
                        }
                        else {
                            r2 = 0.00;
                        }
                        
                        
                        if (JDialogDespotT2.calculateT2) t2Values[k][pixelIndex] = (float) t2;
                        if (JDialogDespotT2.calculateMo) moValues[k][pixelIndex] = (float) mo;
                        if (JDialogDespotT2.invertT2toR2) r2Values[k][pixelIndex] = (float) r2;
                    }
                    else {
                        if (JDialogDespotT2.calculateT2) t2Values[k][pixelIndex] = (float) 0.00;
                        if (JDialogDespotT2.calculateMo) moValues[k][pixelIndex] = (float) 0.00;
                        if (JDialogDespotT2.invertT2toR2) r2Values[k][pixelIndex] = (float) 0.00;
                    }
                    pixelIndex++;
                }
            }
            
            try {
                if (JDialogDespotT2.calculateT2) {
                    t2ResultStack.importData(image.getSliceSize()*k, t2Values[k], true);
                }
                if (JDialogDespotT2.calculateMo) {
                    moResultStack.importData(image.getSliceSize()*k, moValues[k], true);
                }
                if (JDialogDespotT2.invertT2toR2) {
                    r2ResultStack.importData(image.getSliceSize()*k, r2Values[k], true);
                }
            } catch(IOException e) {
                e.printStackTrace();
                MipavUtil.displayError("Data could not be imported into result image");
            }
        }
        
        if (JDialogDespotT2.calculateT2) {
            ViewJFrameImage t2ResultWindow = new ViewJFrameImage(t2ResultStack);
            t2ResultWindow.setTitle("DESPOT2-T2_Map");
            t2ResultWindow.setVisible(true);
        } else if(t2ResultStack != null) {
            t2ResultStack.disposeLocal();
        }
        
        if (JDialogDespotT2.calculateMo) {
            ViewJFrameImage moResultWindow = new ViewJFrameImage(moResultStack);
            moResultWindow.setTitle("DESPOT2_MoMap");
            moResultWindow.setVisible(true);
        } else if(moResultStack != null) {
            moResultStack.disposeLocal();
        }
        
        if (JDialogDespotT2.invertT2toR2) {
            ViewJFrameImage r2ResultWindow = new ViewJFrameImage(r2ResultStack);
            r2ResultWindow.setTitle("DESPOT2-R2Map");
            r2ResultWindow.setVisible(true);
        } else if(r2ResultStack != null) {
            r2ResultStack.disposeLocal();
        }
        
    }
    
    public void calculateT2with180Phase() {
        ModelImage image;
        float[] ctable;
        
        
        double[] fa_phase180;
        double[] scaledFA_phase180;
        
        double[][] ssfpPixelValues_phase180;
        double[] t1PixelValues, b1PixelValues;
        double[] phase180Data;
        
        float[][] t2Values, moValues, r2Values;
        
        double a, d, e2;
        double sumX, sumY, sumXY, sumXX, slope, denominator, intercept, lnslope, t1, t2, e1, mo, r2;
        double x1, x2, y1, y2;
        double[] possibleT2s, possibleMos;
        float noiseSum, threshold;
        
        int width, height, nSlices;
        int x,y,i,j,k,angle, p, ti, p1, p2, pixelIndex, noiseIndex;

        image = ViewUserInterface.getReference().getRegisteredImageByName(wList[ssfpImageIndex_phase180[0]]);
        width = image.getExtents()[0];
        height = image.getExtents()[1];
        if(image.getNDims() > 2) {
            nSlices = image.getExtents()[2];
        } else {
            nSlices = 1;
        }
        
        ssfpPixelValues_phase180 = new double[JDialogDespotT2.Nfa_phase180][width*height];
        t1PixelValues = new double[width*height];
        if (JDialogDespotT2.includeB1Map) b1PixelValues = new double[width*height];
        else b1PixelValues = new double[1];
        
        if (JDialogDespotT2.calculateT2) t2Values = new float[nSlices][width*height];
        else t2Values = new float[1][1];
        if (JDialogDespotT2.calculateMo) moValues = new float[nSlices][width*height];
        else moValues = new float[1][1];
        if (JDialogDespotT2.invertT2toR2) r2Values = new float[nSlices][width*height];
        else r2Values = new float[1][1];
        
        t2ResultStack = new ModelImage(ModelImage.DOUBLE, image.getExtents(), "t2 Results");
        moResultStack = new ModelImage(ModelImage.DOUBLE, image.getExtents(), "mo Results");
        r2ResultStack = new ModelImage(ModelImage.DOUBLE, image.getExtents(), "r2 Results");
        
        t2ResultStack = nearCloneImage(image, t2ResultStack);
        moResultStack = nearCloneImage(image, moResultStack);
        r2ResultStack = nearCloneImage(image, r2ResultStack);
        
        fa_phase180 = new double[JDialogDespotT2.Nfa_phase180];
        scaledFA_phase180 = new double[JDialogDespotT2.Nfa_phase180];
        for (angle=0; angle<JDialogDespotT2.Nfa_phase180; angle++) {
            fa_phase180[angle] = Math.toRadians(despotFA_phase180[angle]);
        }
        
        phase180Data = new double[JDialogDespotT2.Nfa_phase180];
        
        // Actually perform the T2 Calculations
        for (k=0; k<nSlices; k++) {
            fireProgressStateChanged("calculating T2 values on slice: "+k+" of "+nSlices);
            fireProgressStateChanged(0+(int)(((float)k+1.0)/(float)nSlices*90.0));
            if(interrupted()) {
                hardInterrupt = true;
                return;
            }
            // grab the ssfp pixel values from the phase = 180 data
            for (angle=0; angle<JDialogDespotT2.Nfa_phase180; angle++) {
                image = ViewUserInterface.getReference().getRegisteredImageByName(wList[ssfpImageIndex_phase180[angle]]);
                
                pixelIndex = 0;
                for (y=0; y<height; y++) {
                    for (x=0; x<width; x++) {
                        ssfpPixelValues_phase180[angle][pixelIndex] = image.getDouble(x,y, k);
                        pixelIndex++;
                    }
                }
            }
            
            // grab the T1 and B1 information
            image = ViewUserInterface.getReference().getRegisteredImageByName(wList[t1ImageIndex]);
            
            pixelIndex = 0;
            for (y=0; y<height; y++) {
                for (x=0; x<width; x++) {
                    t1PixelValues[pixelIndex] = image.getDouble(x,y, k);
                    pixelIndex++;
                }
            }
            
            if (JDialogDespotT2.includeB1Map == true) {
                image = ViewUserInterface.getReference().getRegisteredImageByName(wList[b1ImageIndex]);
                
                pixelIndex = 0;
                for (y=0; y<height; y++) {
                    for (x=0; x<width; x++) {
                        b1PixelValues[pixelIndex] = image.getDouble(x,y, k);
                        pixelIndex++;
                    }
                }
            }
            
            // now that we have all the information, perform the calculate pixel-wise
            pixelIndex = 0;
            for (x=0; x<width; x++) {
                for (y=0; y<height; y++) {
                    
                    if (t1PixelValues[pixelIndex] > 0.00) {
                        
                        e1 = Math.exp(-JDialogDespotT2.despotTR/t1PixelValues[pixelIndex]);
                        
                        // scale up (or down) the flip angles based on the calculated B1 if required
                        if (JDialogDespotT2.includeB1Map == true) {
                            for (p=0; p<JDialogDespotT2.Nfa_phase180; p++) scaledFA_phase180[p] = fa_phase180[p]*b1PixelValues[pixelIndex];
                        }
                        
                        else {
                            for (p=0; p<JDialogDespotT2.Nfa_phase180; p++) scaledFA_phase180[p] = fa_phase180[p];
                        }
                        
                        // grab the SSFP values for this pixel
                        for (p=0; p<JDialogDespotT2.Nfa_phase180; p++) phase180Data[p] = ssfpPixelValues_phase180[p][pixelIndex];
                        
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
                        
                        for (p=0; p<JDialogDespotT2.Nfa_phase180; p++) {
                            sumX += phase180Data[p]/Math.tan(scaledFA_phase180[p]);
                            sumY += phase180Data[p]/Math.sin(scaledFA_phase180[p]);
                            sumXY += (phase180Data[p]/Math.tan(scaledFA_phase180[p]))*(phase180Data[p]/Math.sin(scaledFA_phase180[p]));
                            sumXX += (phase180Data[p]/Math.tan(scaledFA_phase180[p]))*(phase180Data[p]/Math.tan(scaledFA_phase180[p]));
                        }
                        
                        d = (JDialogDespotT2.Nfa_phase180*sumXX) - sumX*sumX;
                        a = (JDialogDespotT2.Nfa_phase180*sumXY) - (sumX*sumY);
                        
                        if (d != 0) {
                            slope = a/d;
                            denominator = (slope-e1)/(slope*e1-1.00);
                            intercept = (sumY-slope*sumX)/JDialogDespotT2.Nfa_phase180;
                            
                            if (denominator > 0.00 && denominator < 1.00) {
                                t2 = -JDialogDespotT2.despotTR/Math.log(denominator);
                                e2 = Math.exp(-JDialogDespotT2.despotTR/t2);
                                mo = intercept*(1.00-e1*e2)/(1.00-e1);
                            }
                            else {
                                mo = JDialogDespotT2.maxMo;
                                t2 = JDialogDespotT2.maxT2;
                            }
                        }
                        else {
                            mo = JDialogDespotT2.maxMo;
                            t2 = JDialogDespotT2.maxT2;
                        }
                        
                        
                        if (t2 < 0.00 || t2 > JDialogDespotT2.maxT2) {
                            t2 = JDialogDespotT2.maxT2;
                        }
                        if (mo < 0.00 || mo > JDialogDespotT2.maxMo) {
                            mo = JDialogDespotT2.maxMo;
                        }
                                                
                        // invert to r2
                        if (t2 != 0.00) {
                            r2 = 1.00/t2;
                        }
                        else {
                            r2 = 0.00;
                        }
                        
                        
                        if (JDialogDespotT2.calculateT2) t2Values[k][pixelIndex] = (float) t2;
                        if (JDialogDespotT2.calculateMo) moValues[k][pixelIndex] = (float) mo;
                        if (JDialogDespotT2.invertT2toR2) r2Values[k][pixelIndex] = (float) r2;
                    }
                    else {
                        if (JDialogDespotT2.calculateT2) t2Values[k][pixelIndex] = (float) 0.00;
                        if (JDialogDespotT2.calculateMo) moValues[k][pixelIndex] = (float) 0.00;
                        if (JDialogDespotT2.invertT2toR2) r2Values[k][pixelIndex] = (float) 0.00;
                    }
                    pixelIndex++;
                }
            }
            
            try {
                if (JDialogDespotT2.calculateT2) t2ResultStack.importData(image.getSliceSize()*k, t2Values[k], true);
                if (JDialogDespotT2.calculateMo) moResultStack.importData(image.getSliceSize()*k, moValues[k], true);
                if (JDialogDespotT2.invertT2toR2) r2ResultStack.importData(image.getSliceSize()*k, r2Values[k], true);
            } catch(IOException e) {
                e.printStackTrace();
                MipavUtil.displayError("Data could not be imported into result image");
            }
        }
        
        if (JDialogDespotT2.calculateT2) {
            ViewJFrameImage t2ResultWindow = new ViewJFrameImage(t2ResultStack);
            t2ResultWindow.setTitle("DESPOT2_T2_Map");
            t2ResultWindow.setVisible(true);
        } else if(t2ResultStack != null) {
            t2ResultStack.disposeLocal();
        }
        
        if (JDialogDespotT2.calculateMo) {
            ViewJFrameImage moResultWindow = new ViewJFrameImage(moResultStack);
            moResultWindow.setTitle("DESPOT2_MoMap");
            moResultWindow.setVisible(true);
        } else if(moResultStack != null) {
            moResultStack.disposeLocal();
        }
        
        if (JDialogDespotT2.invertT2toR2) {
            ViewJFrameImage r2ResultWindow = new ViewJFrameImage(r2ResultStack);
            r2ResultWindow.setTitle("DESPOT2_R2Map");
            r2ResultWindow.setVisible(true);
        } else if(r2ResultStack != null) {
            r2ResultStack.disposeLocal();
        }
    }
    
    

    public void calculateT2withApproximateModelling() {
        ModelImage image;
        float[] ctable;
        
        
        double[] fa_phase0, fa_phase180;
        double[] scaledFA_phase0, scaledFA_phase180;
        
        double[][] ssfpPixelValues_phase0, ssfpPixelValues_phase180;
        double[] t1PixelValues, b1PixelValues;
        double[] phase0Data, phase180Data;
        
        float[][] t2Values, moValues, r2Values;
        
        double a, d, e2;
        double sumX, sumY, sumXY, sumXX, slope, denominator, intercept, lnslope, t1, t2, e1, mo, r2;
        double x1, x2, y1, y2;
        double[] possibleT2s, possibleMos;
        float noiseSum, threshold;
        float Residuals, guessDiffence;
        float [] lastGuess, recentGuess;
        
        int width, height, nSlices;
        int x,y,i,j,k,angle, p, ti, p1, p2, pixelIndex, noiseIndex;

        image = ViewUserInterface.getReference().getRegisteredImageByName(wList[ssfpImageIndex_phase0[0]]);
        width = image.getExtents()[0];
        height = image.getExtents()[1];
        if(image.getNDims() > 2) {
            nSlices = image.getExtents()[2];
        } else {
            nSlices = 1;
        }
        
        ssfpPixelValues_phase0 = new double[JDialogDespotT2.Nfa_phase0][width*height];
        ssfpPixelValues_phase180 = new double[JDialogDespotT2.Nfa_phase180][width*height];
        t1PixelValues = new double[width*height];
        if (JDialogDespotT2.includeB1Map) b1PixelValues = new double[width*height];
        else b1PixelValues = new double[1];
        
        if (JDialogDespotT2.calculateT2) t2Values = new float[nSlices][width*height];
        else t2Values = new float[1][1];
        if (JDialogDespotT2.calculateMo) moValues = new float[nSlices][width*height];
        else moValues = new float[1][1];
        if (JDialogDespotT2.invertT2toR2) r2Values = new float[nSlices][width*height];
        else r2Values = new float[1][1];
        
        t2ResultStack = new ModelImage(ModelImage.DOUBLE, image.getExtents(), "t2 Results");
        moResultStack = new ModelImage(ModelImage.DOUBLE, image.getExtents(), "mo Results");
        r2ResultStack = new ModelImage(ModelImage.DOUBLE, image.getExtents(), "r2 Results");
        
        t2ResultStack = nearCloneImage(image, t2ResultStack);
        moResultStack = nearCloneImage(image, moResultStack);
        r2ResultStack = nearCloneImage(image, r2ResultStack);
        
        fa_phase0 = new double[JDialogDespotT2.Nfa_phase0];
        fa_phase180 = new double[JDialogDespotT2.Nfa_phase180];
        scaledFA_phase0 = new double[JDialogDespotT2.Nfa_phase0];
        scaledFA_phase180 = new double[JDialogDespotT2.Nfa_phase180];
        for (angle=0; angle<JDialogDespotT2.Nfa_phase0; angle++) {
            fa_phase0[angle] = Math.toRadians(despotFA_phase0[angle]);
        }
        for (angle=0; angle<JDialogDespotT2.Nfa_phase180; angle++) {
            fa_phase180[angle] = Math.toRadians(despotFA_phase180[angle]);
        }
        
        phase0Data = new double[JDialogDespotT2.Nfa_phase0];
        phase180Data = new double[JDialogDespotT2.Nfa_phase180];
        
        possibleT2s = new double[2];
        possibleMos = new double[2];
        
        // Actually perform the T2 Calculations
        for (k=0; k<nSlices; k++) {
            fireProgressStateChanged("calculating T2 values on slice: "+k+" of "+nSlices);
            fireProgressStateChanged(0+(int)(((float)k+1.0)/(float)nSlices*90.0));
            if(interrupted()) {
                hardInterrupt = true;
                return;
            }
            // grab the ssfp pixel values from the phase = 0 data
            for (angle=0; angle<JDialogDespotT2.Nfa_phase0; angle++) {
                image = ViewUserInterface.getReference().getRegisteredImageByName(wList[ssfpImageIndex_phase0[angle]]);
                
                pixelIndex = 0;
                for (y=0; y<height; y++) {
                    for (x=0; x<width; x++) {
                        ssfpPixelValues_phase0[angle][pixelIndex] = image.getDouble(x, y, k);
                        pixelIndex++;
                    }
                }
            }
            
            // grab the ssfp pixel values from the phase = 180 data
            for (angle=0; angle<JDialogDespotT2.Nfa_phase180; angle++) {
                image = ViewUserInterface.getReference().getRegisteredImageByName(wList[ssfpImageIndex_phase180[angle]]);
                
                pixelIndex = 0;
                for (y=0; y<height; y++) {
                    for (x=0; x<width; x++) {
                        ssfpPixelValues_phase180[angle][pixelIndex] = image.getDouble(x, y, k);
                        pixelIndex++;
                    }
                }
            }
            
            // grab the T1 and B1 information
            image = ViewUserInterface.getReference().getRegisteredImageByName(wList[t1ImageIndex]);
            
            pixelIndex = 0;
            for (y=0; y<height; y++) {
                for (x=0; x<width; x++) {
                    t1PixelValues[pixelIndex] = image.getDouble(x, y, k);
                    pixelIndex++;
                }
            }
            
            if (JDialogDespotT2.includeB1Map == true) {
                image = ViewUserInterface.getReference().getRegisteredImageByName(wList[b1ImageIndex]);
                
                pixelIndex = 0;
                for (y=0; y<height; y++) {
                    for (x=0; x<width; x++) {
                        b1PixelValues[pixelIndex] = image.getDouble(x, y, k);
                        pixelIndex++;
                    }
                }
            }
            
            // now that we have all the information, perform the calculate pixel-wise
            pixelIndex = 0;
            for (x=0; x<width; x++) {
                for (y=0; y<height; y++) {
                
                    if (t1PixelValues[pixelIndex] > 0.00) {
                        
                        e1 = Math.exp(-JDialogDespotT2.despotTR/t1PixelValues[pixelIndex]);
                        
                        // scale up (or down) the flip angles based on the calculated B1 if required
                        if (JDialogDespotT2.includeB1Map == true) {
                            for (p=0; p<JDialogDespotT2.Nfa_phase0; p++) scaledFA_phase0[p] = fa_phase0[p]*b1PixelValues[pixelIndex];
                            for (p=0; p<JDialogDespotT2.Nfa_phase180; p++) scaledFA_phase180[p] = fa_phase180[p]*b1PixelValues[pixelIndex];
                        }
                        else {
                            for (p=0; p<JDialogDespotT2.Nfa_phase0; p++) {
                                scaledFA_phase0[p] = fa_phase0[p];
                                scaledFA_phase180[p] = fa_phase180[p];
                            }
                        }
                        
                        // grab the SSFP values for this pixel
                        for (p=0; p<JDialogDespotT2.Nfa_phase0; p++) phase0Data[p] = ssfpPixelValues_phase0[p][pixelIndex];
                        for (p=0; p<JDialogDespotT2.Nfa_phase180; p++) phase180Data[p] = ssfpPixelValues_phase180[p][pixelIndex];
                        
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
                    
                        for (p=0; p<JDialogDespotT2.Nfa_phase0; p++) {
                            sumX += phase0Data[p]/Math.tan(scaledFA_phase0[p]);
                            sumY += phase0Data[p]/Math.sin(scaledFA_phase0[p]);
                            sumXY += (phase0Data[p]/Math.tan(scaledFA_phase0[p]))*(phase0Data[p]/Math.sin(scaledFA_phase0[p]));
                            sumXX += (phase0Data[p]/Math.tan(scaledFA_phase0[p]))*(phase0Data[p]/Math.tan(scaledFA_phase0[p]));
                        }
                        
                        d = (JDialogDespotT2.Nfa_phase0*sumXX) - sumX*sumX;
                        a = (JDialogDespotT2.Nfa_phase0*sumXY) - (sumX*sumY);
                        
                        if (d != 0) {
                            slope = a/d;
                            denominator = (e1-slope)/(1.00-slope*e1);
                            intercept = (sumY-slope*sumX)/JDialogDespotT2.Nfa_phase0;
                            
                            if (denominator > 0.00 && denominator < 1.00) {
                                t2 = -JDialogDespotT2.despotTR/Math.log(denominator);
                                e2 = Math.exp(-JDialogDespotT2.despotTR/t2);
                                mo = intercept*(1.00-e1*e2)/(1.00-e1);
                            }
                            else {
                                mo = 0.00;
                                t2 = 0.00;
                            }
                        }
                        else {
                            mo = 0.00;
                            t2 = 0.00;
                        }
                        

                        if (t2 < 0.00 || t2 > JDialogDespotT2.maxT2) {
                            t2 = JDialogDespotT2.maxT2;
                        }
                        if (mo < 0.00 || mo > JDialogDespotT2.maxMo) {
                            mo = 0.00;
                        }
                    
            
                        possibleT2s[0] = t2;
                        possibleMos[0] = mo;
                        
                        
                        // calculate T2 first from the phase = 180 data
                        sumX = 0.00;
                        sumY = 0.00;
                        sumXY = 0.00;
                        sumXX = 0.00;
                        
                        for (p=0; p<JDialogDespotT2.Nfa_phase180; p++) {
                            sumX += phase180Data[p]/Math.tan(scaledFA_phase180[p]);
                            sumY += phase180Data[p]/Math.sin(scaledFA_phase180[p]);
                            sumXY += (phase180Data[p]/Math.tan(scaledFA_phase180[p]))*(phase180Data[p]/Math.sin(scaledFA_phase180[p]));
                            sumXX += (phase180Data[p]/Math.tan(scaledFA_phase180[p]))*(phase180Data[p]/Math.tan(scaledFA_phase180[p]));
                        }
                        
                        d = (JDialogDespotT2.Nfa_phase180*sumXX) - sumX*sumX;
                        a = (JDialogDespotT2.Nfa_phase180*sumXY) - (sumX*sumY);
                        
                        if (d != 0) {
                            slope = a/d;
                            denominator = (slope-e1)/(slope*e1-1.00);
                            intercept = (sumY-slope*sumX)/JDialogDespotT2.Nfa_phase180;
                            
                            if (denominator > 0.00 && denominator < 1.00) {
                                t2 = -JDialogDespotT2.despotTR/Math.log(denominator);
                                e2 = Math.exp(-JDialogDespotT2.despotTR/t2);
                                mo = intercept*(1.00-e1*e2)/(1.00-e1);
                            }
                            else {
                                mo = 0.00;
                                t2 = 0.00;
                            }
                        }
                        else {
                            mo = 0.00;
                            t2 = 0.00;
                        }
                        
                        
                        if (t2 < 0.00 || t2 > JDialogDespotT2.maxT2) {
                            t2 = JDialogDespotT2.maxT2;
                        }
                        if (mo < 0.00 || mo > JDialogDespotT2.maxMo) {
                            mo = 0.00;
                        }
                        
                        possibleT2s[1] = t2;
                        possibleMos[1] = mo;
                    
                        
                        // now, choose the maximum T2 and the corresponding mo value
                        if (possibleT2s[0] >= possibleT2s[1]) {
                            t2 = possibleT2s[0];
                            mo = possibleMos[0];
                        }
                        else {
                            t2 = possibleT2s[1];
                            mo = possibleMos[1];
                        }
                         
                        
                        //t2 = (possibleT2s[0]+possibleT2s[1])/2.00;
                        //mo = (possibleMos[0]+possibleMos[1])/2.00;
                        
                        
                        // invert to r2
                        if (t2 != 0.00) {
                            r2 = 1.00/t2;
                        }
                        else {
                            r2 = 0.00;
                        }
                    
                        
                        if (JDialogDespotT2.calculateT2) t2Values[k][pixelIndex] = (float) t2;
                        if (JDialogDespotT2.calculateMo) moValues[k][pixelIndex] = (float) mo;
                        if (JDialogDespotT2.invertT2toR2) r2Values[k][pixelIndex] = (float) r2;
                    }
                    else {
                        if (JDialogDespotT2.calculateT2) t2Values[k][pixelIndex] = (float) 0.00;
                        if (JDialogDespotT2.calculateMo) moValues[k][pixelIndex] = (float) 0.00;
                        if (JDialogDespotT2.invertT2toR2) r2Values[k][pixelIndex] = (float) 0.00;
                    }
                    pixelIndex++;
                }
            }
            
            try {
                if (JDialogDespotT2.calculateT2) t2ResultStack.importData(image.getSliceSize()*k, t2Values[k], true);
                if (JDialogDespotT2.calculateMo) moResultStack.importData(image.getSliceSize()*k, moValues[k], true);
                if (JDialogDespotT2.invertT2toR2) r2ResultStack.importData(image.getSliceSize()*k, r2Values[k], true);
            } catch(IOException e) {
                e.printStackTrace();
                MipavUtil.displayError("Data could not be imported into result image");
            }
        }
        
        if (JDialogDespotT2.calculateT2) {
            ViewJFrameImage t2ResultWindow = new ViewJFrameImage(t2ResultStack);
            t2ResultWindow.setTitle("CalculatedT2Map_AM");
            t2ResultWindow.setVisible(true);
        } else if(t2ResultStack != null) {
            t2ResultStack.disposeLocal();
        }
        
        if (JDialogDespotT2.calculateMo) {
            ViewJFrameImage moResultWindow = new ViewJFrameImage(moResultStack);
            moResultWindow.setTitle("CalculatedMoMap_AM");
            moResultWindow.setVisible(true);
        } else if(moResultStack != null) {
            moResultStack.disposeLocal();
        }
        
        if (JDialogDespotT2.invertT2toR2) {
            ViewJFrameImage r2ResultWindow = new ViewJFrameImage(r2ResultStack);
            r2ResultWindow.setTitle("CalculatedR2Map_AM");
            r2ResultWindow.setVisible(true);
        } else if(r2ResultStack != null) {
            r2ResultStack.disposeLocal();
        }
    }
    
    public void calculateT2withFullModelling() {
        fireProgressStateChanged("prepping data - hang on");
        ModelImage image;
        float[] ctable;
        
        double[] FA, scaledFA, phaseIncrements, sina, cosa;
        
        double[][] ssfpPixelValues_phase0, ssfpPixelValues_phase180;
        double[] t1PixelValues, b1PixelValues;
        double[] ssfpSampleData;
        
        float[][] boField, moField, t2Field, r2Field;
        double[][][] t2Values, moValues, r2Values, boValues;
        double[][] Gaussian;
        double smoothedBo;
        
        
        double[] optimization, initialGuess;
        double[] twoPOptimization, twoPInitialGuess;
        double Residuals, guessDifference, lowestResiduals;
        double[] lastGuess, recentGuess, bestGuess;
        int restartIndex;
        int numParams, numVertices;
        
        double offResonanceMod, resonancePeriod;
        int repetitionCycle;
        
        optimization = new double[3];
        initialGuess = new double[3];
        twoPOptimization = new double[2];
        twoPInitialGuess = new double[2];
        
        lastGuess = new double[3];
        recentGuess = new double[3];
        bestGuess = new double[3];
        
        int iterations;
        
        double t2, mo, bo, r2;
        double rtol; 
        
        double t1, tr;
        
        int width, height, nSlices;
        int x,y,i,j,k,angle, p, pixelIndex, p1,p2;
        
        image = ViewUserInterface.getReference().getRegisteredImageByName(wList[ssfpImageIndex_phase180[0]]);
        width = image.getExtents()[0];
        height = image.getExtents()[1];
        if(image.getNDims() > 2) {
            nSlices = image.getExtents()[2];
        } else {
            nSlices = 1;
        }
        
        ssfpPixelValues_phase0 = new double[JDialogDespotT2.Nfa_phase0][width*height];
        ssfpPixelValues_phase180 = new double[JDialogDespotT2.Nfa_phase180][width*height];
        t1PixelValues = new double[width*height];
        if (JDialogDespotT2.includeB1Map) b1PixelValues = new double[width*height];
        else b1PixelValues = new double[1];
        
        t2ResultStack = new ModelImage(ModelImage.DOUBLE, image.getExtents(), "t2 Results");
        moResultStack = new ModelImage(ModelImage.DOUBLE, image.getExtents(), "mo Results");
        r2ResultStack = new ModelImage(ModelImage.DOUBLE, image.getExtents(), "r2 Results");
        boResultStack = new ModelImage(ModelImage.DOUBLE, image.getExtents(), "bo Results");
        
        t2ResultStack = nearCloneImage(image, t2ResultStack);
        moResultStack = nearCloneImage(image, moResultStack);
        r2ResultStack = nearCloneImage(image, r2ResultStack);
        boResultStack = nearCloneImage(image, boResultStack);
        
        FA = new double[JDialogDespotT2.Nfa_phase0 + JDialogDespotT2.Nfa_phase180];
        scaledFA = new double[JDialogDespotT2.Nfa_phase0 + JDialogDespotT2.Nfa_phase180];
        phaseIncrements = new double[JDialogDespotT2.Nfa_phase0 + JDialogDespotT2.Nfa_phase180];
        sina = new double[JDialogDespotT2.Nfa_phase0 + JDialogDespotT2.Nfa_phase180];
        cosa = new double[JDialogDespotT2.Nfa_phase0 + JDialogDespotT2.Nfa_phase180];
        
        for (angle=0; angle<JDialogDespotT2.Nfa_phase0; angle++) {
            FA[angle] = Math.toRadians(despotFA_phase0[angle]);
            phaseIncrements[angle] = 0.00;
        }
        for (angle=0; angle<JDialogDespotT2.Nfa_phase180; angle++) {
            FA[angle+JDialogDespotT2.Nfa_phase0] = Math.toRadians(despotFA_phase180[angle]);
            phaseIncrements[angle+JDialogDespotT2.Nfa_phase0] = 3.14159265;
        }
        
        ssfpSampleData = new double[JDialogDespotT2.Nfa_phase0 + JDialogDespotT2.Nfa_phase180];
        
        tr = JDialogDespotT2.despotTR;
        resonancePeriod = 1000.00/tr;
        
        t2Values = new double[nSlices][height][width];
        moValues = new double[nSlices][height][width];
        boValues = new double[nSlices][height][width];
        r2Values = new double[nSlices][height][width];
        
        boField = new float[nSlices][width*height];
        moField = new float[nSlices][width*height];
        t2Field = new float[nSlices][width*height];
        r2Field = new float[nSlices][width*height];
        
        Gaussian = new double[5][5];
        
        // define the Gaussian kernel
        Gaussian[0][0] = 0;
        Gaussian[0][1] = 0;
        Gaussian[0][2] = 1;
        Gaussian[0][3] = 0;
        Gaussian[0][4] = 0;
        Gaussian[1][0] = 0;
        Gaussian[1][1] = 2;
        Gaussian[1][2] = 4;
        Gaussian[1][3] = 2;
        Gaussian[1][4] = 0;
        Gaussian[2][0] = 1;
        Gaussian[2][1] = 4;
        Gaussian[2][2] = 6;
        Gaussian[2][3] = 4;
        Gaussian[2][4] = 1;
        Gaussian[3][0] = 0;
        Gaussian[3][1] = 2;
        Gaussian[3][2] = 4;
        Gaussian[3][3] = 2;
        Gaussian[3][4] = 0;
        Gaussian[0][0] = 0;
        Gaussian[4][1] = 0;
        Gaussian[4][2] = 1;
        Gaussian[4][3] = 0;
        Gaussian[4][4] = 0;
        
        
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

        // Perform an initial T2 Calculation to get the rough Bo field
        for (k=0; k<nSlices; k++) {
            fireProgressStateChanged("calculating Initial Estimates for slice: "+k+" of "+nSlices);
            fireProgressStateChanged(0+(int)(((float)k+1.0)/(float)nSlices*50.0));
            if(interrupted()) {
                hardInterrupt = true;
                return;
            }
            // grab the ssfp pixel values from the phase = 0 data
            for (angle=0; angle<JDialogDespotT2.Nfa_phase0; angle++) {
                image = ViewUserInterface.getReference().getRegisteredImageByName(wList[ssfpImageIndex_phase0[angle]]);
                
                pixelIndex = 0;
                for (y=0; y<height; y++) {
                    for (x=0; x<width; x++) {
                        ssfpPixelValues_phase0[angle][pixelIndex] = image.getDouble(x, y, k);
                        pixelIndex++;
                    }
                }
            }
            
            // grab the ssfp pixel values from the phase = 180 data
            for (angle=0; angle<JDialogDespotT2.Nfa_phase180; angle++) {
                image = ViewUserInterface.getReference().getRegisteredImageByName(wList[ssfpImageIndex_phase180[angle]]);
                
                pixelIndex = 0;
                for (y=0; y<height; y++) {
                    for (x=0; x<width; x++) {
                        ssfpPixelValues_phase180[angle][pixelIndex] = image.getDouble(x, y, k);
                        pixelIndex++;
                    }
                }
            }
            
            // grab the T1 and B1 information
            image = ViewUserInterface.getReference().getRegisteredImageByName(wList[t1ImageIndex]);
            
            pixelIndex = 0;
            for (y=0; y<height; y++) {
                for (x=0; x<width; x++) {
                    t1PixelValues[pixelIndex] = image.getDouble(x, y, k);
                    pixelIndex++;
                }
            }
            
            if (JDialogDespotT2.includeB1Map == true) {
                image = ViewUserInterface.getReference().getRegisteredImageByName(wList[b1ImageIndex]);
                
                pixelIndex = 0;
                for (y=0; y<height; y++) {
                    for (x=0; x<width; x++) {
                        b1PixelValues[pixelIndex] = image.getDouble(x, y, k);
                        pixelIndex++;
                    }
                }
            }
            
            // now that we have all the information, perform the calculate the initial estimates of B1, T2 and Mo pixel-wise
            pixelIndex = 0;
            
            for (y=0; y<height; y++) {
                for (x=0; x<width; x++) {
                    
                    t1 = t1PixelValues[pixelIndex];
                    
                    if (t1 > 10.00) {
                        /*
                        if (t1 > 2500) {
                            boValues[k-1][y][x] = 0.00;
                            moValues[k-1][y][x] = maxMo;
                            t2Values[k-1][y][x] = maxT2;
                            r2Values[k-1][y][x] = 1.00/maxT2;
                            
                        }
                         
                        else {
                         */
                        
                        // scale up (or down) the flip angles based on the calculated B1 if required
                        if (JDialogDespotT2.includeB1Map == true) {
                            for (p=0; p<JDialogDespotT2.Nfa_phase0+JDialogDespotT2.Nfa_phase180; p++) scaledFA[p] = FA[p]*b1PixelValues[pixelIndex];
                        }
                        else {
                            for (p=0; p<JDialogDespotT2.Nfa_phase0+JDialogDespotT2.Nfa_phase180; p++) scaledFA[p] = FA[p];
                        }
                        
                        // calculate the sina and cosa matrices
                        for (p=0; p<JDialogDespotT2.Nfa_phase0+JDialogDespotT2.Nfa_phase180; p++) {
                            sina[p] = Math.sin(scaledFA[p]);
                            cosa[p] = Math.cos(scaledFA[p]);
                        }
                        
                        // grab the SSFP values for this pixel
                        for (p=0; p<JDialogDespotT2.Nfa_phase0; p++) ssfpSampleData[p] = ssfpPixelValues_phase0[p][pixelIndex];
                        for (p=0; p<JDialogDespotT2.Nfa_phase180; p++) ssfpSampleData[p+JDialogDespotT2.Nfa_phase0] = ssfpPixelValues_phase180[p][pixelIndex];
                        
                        // begin with the downhill simplex
                    
                        // calculate an initial guess for Mo, T2 and Bo
                        if (JDialogDespotT2.geScanner) {
                            initialGuess[0] = 10000.00; // initial guess for mo
                            initialGuess[1] = 100.00;   // initial guess for t2
                            initialGuess[2] = 500.0;    // initial guess for off-resonance (Hz)
                        }
                        else {
                            initialGuess[0] = 1000.00;
                            initialGuess[1] = 100.00;
                            initialGuess[2] = 500.0;
                        }
                            
                        
                        threePDownHillSimplex(optimization, initialGuess, t1, 
                                tr, ssfpSampleData, sina, cosa, phaseIncrements, JDialogDespotT2.Nfa_phase0+JDialogDespotT2.Nfa_phase180);
                        
                        
                        mo = optimization[0];
                        t2 = optimization[1];
                        bo = optimization[2];
                        r2 = 0.00;
                        
                        if (mo < 0.00) mo = -1.00*mo;
                        if (t2 < 0.00) t2 = -1.00*t2;
                        if (bo < 0.00) bo = -1.00*bo;
                        
                        if (t2 > JDialogDespotT2.maxT2) {
                            t2 = JDialogDespotT2.maxT2;
                        }
                        
                        // invert to r2
                        if (t2 != 0.00) {
                            r2 = 1.00/t2;
                        }
                        else {
                            r2 = 0.00;
                        }
                        
                        
                        boValues[k][y][x] = bo;
                        moValues[k][y][x] = mo;
                        t2Values[k][y][x] = t2;
                        if (t1 > 0) r2Values[k][y][x] = 1.00/t2;
                        else r2Values[k][y][x] = 0.00;
                        //}
                    }
                    else {
                        boValues[k][y][x] = 0.00;
                        moValues[k][y][x] = 0.00;
                        t2Values[k][y][x] = 0.00;
                        r2Values[k][y][x] = 0.00;
                    }
                    
                    pixelIndex++;
                    
                } // close y (height) loop
            } // close x (width) loop
        } // close the k (slice) loop


        // now, go back through and smooth the Bo field 
        for (k=0; k<nSlices; k++) {
            fireProgressStateChanged("smoothing B0 field on slice: "+k+" of "+nSlices);
            fireProgressStateChanged(50+(int)(((float)k+1.0)/(float)nSlices*25.0));
            if(interrupted()) {
                hardInterrupt = true;
                return;
            }
            pixelIndex = 0;
            for (y=0; y<height; y++) {
                for (x=0; x<width; x++) {
                    
                    if (y>2 && y<height-2 && x>2 && x<width-2) {
                        
                        smoothedBo = 0.00;
                        for (p1=0; p1<5; p1++) {
                            for (p2=0; p2<5; p2++) smoothedBo += boValues[k][y-2+p2][x-2+p1]*Gaussian[p1][p2];
                        }
                        smoothedBo = smoothedBo / 34.00;
                        
                        boField[k][pixelIndex] = (float) smoothedBo;
                    }
                    else boField[k][pixelIndex] = (float) boValues[k][y][x];
                    
                    pixelIndex ++;
                }
            }
            
        }
        
        // now, go back through and calculate T2 and Mo again using the smoothed Bo value
        for (k=0; k<nSlices; k++) {
            fireProgressStateChanged("recalculating values on slice: "+k+" of "+nSlices);
            fireProgressStateChanged(75+(int)(((float)k+1.0)/(float)nSlices*15.0));
            if(interrupted()) {
                hardInterrupt = true;
                return;
            }
            // grab the ssfp pixel values from the phase = 0 data
            for (angle=0; angle<JDialogDespotT2.Nfa_phase0; angle++) {
                image = ViewUserInterface.getReference().getRegisteredImageByName(wList[ssfpImageIndex_phase0[angle]]);
                
                pixelIndex = 0;
                for (y=0; y<height; y++) {
                    for (x=0; x<width; x++) {
                        ssfpPixelValues_phase0[angle][pixelIndex] = image.getDouble(x, y, k);
                        pixelIndex++;
                    }
                }
            }
            
            // grab the ssfp pixel values from the phase = 180 data
            for (angle=0; angle<JDialogDespotT2.Nfa_phase180; angle++) {
                image = ViewUserInterface.getReference().getRegisteredImageByName(wList[ssfpImageIndex_phase180[angle]]);
                
                pixelIndex = 0;
                for (y=0; y<height; y++) {
                    for (x=0; x<width; x++) {
                        ssfpPixelValues_phase180[angle][pixelIndex] = image.getDouble(x, y, k);
                        pixelIndex++;
                    }
                }
            }
            
            // grab the T1 and B1 information
            image = ViewUserInterface.getReference().getRegisteredImageByName(wList[t1ImageIndex]);
            
            pixelIndex = 0;
            for (y=0; y<height; y++) {
                for (x=0; x<width; x++) {
                    t1PixelValues[pixelIndex] = image.getDouble(x, y, k);
                    pixelIndex++;
                }
            }
            
            if (JDialogDespotT2.includeB1Map == true) {
                image = ViewUserInterface.getReference().getRegisteredImageByName(wList[b1ImageIndex]);
                
                pixelIndex = 0;
                for (y=0; y<height; y++) {
                    for (x=0; x<width; x++) {
                        b1PixelValues[pixelIndex] = image.getDouble(x, y, k);
                        pixelIndex++;
                    }
                }
            }
            
            
            pixelIndex = 0;
            for (y=0; y<height; y++) {
                for (x=0; x<width; x++) {
                    
                    if (boField[k][pixelIndex] > 0.00) {
            
                    
                        bo = boField[k][pixelIndex];
                        t1 = t1PixelValues[pixelIndex];
                        
                                
                        // scale up (or down) the flip angles based on the calculated B1 if required
                        if (JDialogDespotT2.includeB1Map == true) {
                            for (p=0; p<JDialogDespotT2.Nfa_phase0+JDialogDespotT2.Nfa_phase180; p++) scaledFA[p] = FA[p]*b1PixelValues[pixelIndex];
                        }
                        else {
                            for (p=0; p<JDialogDespotT2.Nfa_phase0+JDialogDespotT2.Nfa_phase180; p++) scaledFA[p] = FA[p];
                        }
                        
                        // calculate the sina and cosa matrices
                        for (p=0; p<JDialogDespotT2.Nfa_phase0+JDialogDespotT2.Nfa_phase180; p++) {
                            sina[p] = Math.sin(scaledFA[p]);
                            cosa[p] = Math.cos(scaledFA[p]);
                        }
                        
                        // grab the SSFP values for this pixel
                        for (p=0; p<JDialogDespotT2.Nfa_phase0; p++) ssfpSampleData[p] = ssfpPixelValues_phase0[p][pixelIndex];
                        for (p=0; p<JDialogDespotT2.Nfa_phase180; p++) ssfpSampleData[p+JDialogDespotT2.Nfa_phase0] = ssfpPixelValues_phase180[p][pixelIndex];
                        
                        
                        twoPInitialGuess[0] = moValues[k][y][x];
                        twoPInitialGuess[1] = t2Values[k][y][x];
                        
                        twoPDownHillSimplex(twoPOptimization, twoPInitialGuess, bo, t1, tr, ssfpSampleData, sina, cosa, phaseIncrements, JDialogDespotT2.Nfa_phase0+JDialogDespotT2.Nfa_phase180);
                        
                        mo = twoPOptimization[0];
                        t2 = twoPOptimization[1];
                        
                        if (mo < 0.00) mo = -1.00*mo;
                        if (t2 < 0.00) t2 = -1.00*t2;
                        
                        if (t2 > JDialogDespotT2.maxT2) t2 = JDialogDespotT2.maxT2;
                        
                        if (t2 > 0.00) r2 = 1.00/t2;
                        else r2 = 0.00;
                        
                        t2Field[k][pixelIndex] = (float) t2;
                        moField[k][pixelIndex] = (float) mo;
                        r2Field[k][pixelIndex] = (float) r2;
                    }
                    else {
                        t2Field[k][pixelIndex] = (float) 0.00;
                        moField[k][pixelIndex] = (float) 0.00;
                        r2Field[k][pixelIndex] = (float) 0.00;
                    }
                    
                    pixelIndex++;
                    
                } // close y (height) loop
            } // close x (widht) loop
            
            try {
                // add data to the final stacks
                if (JDialogDespotT2.calculateT2) t2ResultStack.importData(image.getSliceSize()*k, t2Field[k], true);
                if (JDialogDespotT2.calculateMo) moResultStack.importData(image.getSliceSize()*k, moField[k], true);
                if (JDialogDespotT2.invertT2toR2) r2ResultStack.importData(image.getSliceSize()*k, r2Field[k], true);
                if (JDialogDespotT2.calculateBo) boResultStack.importData(image.getSliceSize()*k, boField[k], true);
            } catch(IOException e) {
                e.printStackTrace();
                MipavUtil.displayError("Data could not be imported into result image");
            }// end the k (slice) loop 
        }
        
        if (JDialogDespotT2.calculateT2) {
            ViewJFrameImage t2ResultWindow = new ViewJFrameImage(t2ResultStack);
            t2ResultWindow.setTitle("CalculatedT2Map_FM");
            t2ResultWindow.setVisible(true);
        } else if(t2ResultStack != null) {
            t2ResultStack.disposeLocal();
        }
        
        if (JDialogDespotT2.calculateMo) {
            ViewJFrameImage moResultWindow = new ViewJFrameImage(moResultStack);
            moResultWindow.setTitle("CalculatedMoMap_FM");
            moResultWindow.setVisible(true);
        } else if(moResultStack != null) {
            moResultStack.disposeLocal();
        }
        
        if (JDialogDespotT2.invertT2toR2) {
            ViewJFrameImage r2ResultWindow = new ViewJFrameImage(r2ResultStack);
            r2ResultWindow.setTitle("CalculatedR2Map_FM");
            r2ResultWindow.setVisible(true);
        } else if(r2ResultStack != null) {
            r2ResultStack.disposeLocal();
        }
        
        
        if (JDialogDespotT2.calculateBo) {
            ViewJFrameImage boResultWindow = new ViewJFrameImage(boResultStack);
            boResultWindow.setTitle("CalculatedOffResonanceMap_FM");
            boResultWindow.setVisible(true);
        } else if(boResultStack != null) {
            boResultStack.disposeLocal();
        }
    }
    
public void twoPDownHillSimplex(double[] optimization, double[] initialGuess, double Bo, double t1, double tr, double[] ssfpSampleData, double[] sina, double[] cosa, double[] phaseIncrements, int N) {
        
        double RHO, CHI, PSI, SIGMA, maxError, usual_delta, zero_term_delta, residual;
        int best, worst, secondWorst;
        
        
        int NMAX, numParams, numVertices, iterations;
        
        double t2, mo;
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
        residual = calculateTwoPResiduals(twoPSimplexLineValues, Bo, t1, tr, ssfpSampleData, sina, cosa, phaseIncrements, N);
        
        for (i=0; i<numParams; i++) twoPSimplex[0][i] = twoPSimplexLineValues[i];
        twoPSimplex[0][numParams] = residual;
        
        for (i=0; i<numParams; i++) {
            twoPSimplexLineValues[0] = initialGuess[0];
            twoPSimplexLineValues[1] = initialGuess[1];
            
            if (twoPSimplexLineValues[i] != 0.00) twoPSimplexLineValues[i] = (1.00+usual_delta)*twoPSimplexLineValues[i];
            else twoPSimplexLineValues[i] = zero_term_delta;
            residual = calculateTwoPResiduals(twoPSimplexLineValues, Bo, t1, tr, ssfpSampleData, sina, cosa, phaseIncrements, N);
            
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
            twoPReflection[numParams] = calculateTwoPResiduals(twoPReflection, Bo, t1, tr, ssfpSampleData, sina, cosa, phaseIncrements, N);
            
            
            // compare the twoPReflection vertex with the best vertex in the exisiting twoPSimplex
            if (twoPReflection[numParams] < twoPSimplex[best][numParams]) {
                // if the twoPReflection was better, try an twoPExpansion in this direction and see how that is
                //for (i=0; i<numVertices; i++) twoPExpansion[i] = 0.00;
                for (i=0; i<numParams; i++) twoPExpansion[i] = (1.00+RHO*CHI)*twoPSimplexCentre[i] - RHO*CHI*twoPSimplex[worst][i];
                twoPExpansion[numParams] = calculateTwoPResiduals(twoPExpansion, Bo, t1, tr, ssfpSampleData, sina, cosa, phaseIncrements, N);
                
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
                        twoPContraction[numParams] = calculateTwoPResiduals(twoPContraction, Bo, t1, tr, ssfpSampleData, sina, cosa, phaseIncrements, N);
                        
                        if (twoPContraction[numParams] <= twoPReflection[numParams]) {
                            for (i=0; i<numVertices; i++) twoPSimplex[worst][i] = twoPContraction[i];
                        }
                        else {
                            // perform a twoPShrink of all vertices except the best
                            for (j=0; j<numVertices; j++) {
                                if (j != best) {
                                    //for (i=0; i<numVertices; i++) twoPShrink[i] = 0.00;
                                    for (i=0; i<numParams; i++) twoPShrink[i] = twoPSimplex[best][i] + SIGMA*(twoPSimplex[j][i]-twoPSimplex[best][i]);
                                    twoPShrink[numParams] = calculateTwoPResiduals(twoPShrink, Bo, t1, tr, ssfpSampleData, sina, cosa, phaseIncrements, N);
                                    for (i=0; i<numVertices; i++) twoPSimplex[j][i] = twoPShrink[i];
                                }
                            }
                        }
                    }
                    else {
                        // perform an inside twoPContraction
                        //for (i=0; i<numVertices; i++) twoPContraction[i] = 0.00;
                        for (i=0; i<numParams; i++) twoPContraction[i] = (1.00-PSI)*twoPSimplexCentre[i] + PSI*twoPSimplex[worst][i];
                        twoPContraction[numParams] = calculateTwoPResiduals(twoPContraction, Bo, t1, tr, ssfpSampleData, sina, cosa, phaseIncrements, N);
                        
                        if (twoPContraction[numParams] < twoPSimplex[worst][numParams]) {
                            for (i=0; i<numVertices; i++) twoPSimplex[worst][i] = twoPContraction[i];
                        }
                        else {
                            // perform a twoPShrink of all vertices except the best
                            for (j=0; j<numVertices; j++) {
                                if (j != best) {
                                    //for (i=0; i<numVertices; i++) twoPShrink[i] = 0.00;
                                    for (i=0; i<numParams; i++) twoPShrink[i] = twoPSimplex[best][i] + SIGMA*(twoPSimplex[j][i]-twoPSimplex[best][i]);
                                    twoPShrink[numParams] = calculateTwoPResiduals(twoPShrink, Bo, t1, tr, ssfpSampleData, sina, cosa, phaseIncrements, N);
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
        
        mo = twoPSimplex[best][0];
        t2 = twoPSimplex[best][1];
        
        if (mo < 0.00) mo = -1.00*mo;
        if (t2 < 0.00) t2 = -1.00*t2;
        
        optimization[0] = mo;
        optimization[1] = t2;
        
        return;
    }
    
    public void threePDownHillSimplex(double[] optimization, double[] initialGuess, double t1, double tr, double[] ssfpSampleData, double[] sina, double[] cosa, double[] phaseIncrements, int N) {
        
        double RHO, CHI, PSI, SIGMA, maxError, usual_delta, zero_term_delta, residual;
        int best, worst, secondWorst;
        
        
        int NMAX, numParams, numVertices, iterations;
        
        double t2, mo, bo;
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
        
        mo = simplex[best][0];
        t2 = simplex[best][1];
        bo = simplex[best][2];
        
        if (mo < 0.00) mo = -1.00*mo;
        if (t2 < 0.00) t2 = -1.00*t2;
        if (bo < 0.00) bo = -1.00*bo;
        
        optimization[0] = mo;
        optimization[1] = t2;
        optimization[2] = bo;
        
        return;
    }
    
    
    public double calculateTwoPResiduals(double[] simplexLineValues, double bo, double t1, double tr, double[] Signal, double[] sina, double[] cosa, double[] phaseIncrements, int N) {
        
        double mo, e1, e2, phasePrecession, pi, mx, my, guessSignal, residualValue;
        int i;
        
        pi = 3.14159265;
        mo = simplexLineValues[0];
        e1 = Math.exp(-tr/t1);
        e2 = Math.exp(-tr/simplexLineValues[1]);
        
        residualValue = 0.00;
        for (i=0; i<N; i++) {
            phasePrecession = phaseIncrements[i] + 2*pi*((tr/1000.00)*bo);
            
            mx = mo*(1.00-e1)*e2*sina[i]*(Math.cos(phasePrecession)-e2)/( (1.00-e1*cosa[i])*(1.00-e2*Math.cos(phasePrecession))-e2*(e1-cosa[i])*(e2-Math.cos(phasePrecession)) );
            my = mo*(1.00-e1)*e2*sina[i]*Math.sin(phasePrecession)/( (1.00-e1*cosa[i])*(1.00-e2*Math.cos(phasePrecession))-e2*(e1-cosa[i])*(e2-Math.cos(phasePrecession)) );
            
            guessSignal = Math.sqrt( Math.pow(mx,2.00) + Math.pow(my,2.00) );
            
            residualValue += Math.pow( (Signal[i]-guessSignal),2.00 );
        }
        
        return residualValue;
    }
    
    public double calculateResiduals(double[] simplexLineValues, double t1, double tr, double[] Signal, double[] sina, double[] cosa, double[] phaseIncrements, int N) {
        
        double mo, e1, e2, phasePrecession, pi, mx, my, guessSignal, residualValue;
        int i;
        
        pi = 3.14159265;
        mo = simplexLineValues[0];
        e1 = Math.exp(-tr/t1);
        e2 = Math.exp(-tr/simplexLineValues[1]);
        
        residualValue = 0.00;
        for (i=0; i<N; i++) {
            phasePrecession = phaseIncrements[i] + 2*pi*((tr/1000.00)*simplexLineValues[2]);
            
            mx = mo*(1.00-e1)*e2*sina[i]*(Math.cos(phasePrecession)-e2)/( (1.00-e1*cosa[i])*(1.00-e2*Math.cos(phasePrecession))-e2*(e1-cosa[i])*(e2-Math.cos(phasePrecession)) );
            my = mo*(1.00-e1)*e2*sina[i]*Math.sin(phasePrecession)/( (1.00-e1*cosa[i])*(1.00-e2*Math.cos(phasePrecession))-e2*(e1-cosa[i])*(e2-Math.cos(phasePrecession)) );
            
            guessSignal = Math.sqrt( Math.pow(mx,2.00) + Math.pow(my,2.00) );
            
            residualValue += Math.pow( (Signal[i]-guessSignal),2.00 );
        }
        
        return residualValue;
    }
    
    public double calculate2PResiduals(double[] simplexLineValues, double bo, double t1, double tr, double[] Signal, double[] sina, double[] cosa, double[] phaseIncrements, int N) {
        
        double mo, e1, e2, phasePrecession, pi, mx, my, guessSignal, residualValue;
        int i;
        
        pi = 3.14159265;
        mo = simplexLineValues[0];
        e1 = Math.exp(-tr/t1);
        e2 = Math.exp(-tr/simplexLineValues[1]);
        
        residualValue = 0.00;
        for (i=0; i<N; i++) {
            phasePrecession = phaseIncrements[i] + 2*pi*((tr/1000.00)*bo);
            
            mx = mo*(1.00-e1)*e2*sina[i]*(Math.cos(phasePrecession)-e2)/( (1.00-e1*cosa[i])*(1.00-e2*Math.cos(phasePrecession))-e2*(e1-cosa[i])*(e2-Math.cos(phasePrecession)) );
            my = mo*(1.00-e1)*e2*sina[i]*Math.sin(phasePrecession)/( (1.00-e1*cosa[i])*(1.00-e2*Math.cos(phasePrecession))-e2*(e1-cosa[i])*(e2-Math.cos(phasePrecession)) );
            
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
    
    public void reduceBoField(double[][] boField, double resonancePeriod, int width, int height) {
        int i,j;
        double fraction, offResonanceMod;
        int repetitionCycle;
        
        for (i=0; i<width; i++) {
            for (j=0; j<height; j++) {
                
                repetitionCycle = 0;
                
                fraction = boField[i][j]/resonancePeriod;
                if (fraction >= 1) repetitionCycle = (int) (Math.floor(fraction));
                if (fraction < 1 && fraction >= 0.5) repetitionCycle = 1;
                if (fraction < 0.5) repetitionCycle = 0;
                
                offResonanceMod = boField[i][j] - (repetitionCycle*resonancePeriod);
                if (offResonanceMod < 0.00) offResonanceMod = -1.00*offResonanceMod;
                
                boField[i][j] = offResonanceMod;
                
                
            }
        }
        
        return;
    }
    
    
    public void smoothField(double[][] field, float[][] fieldValues, int width, int height, int k, double[][] Gaussian) {
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
    
    public void smoothFieldB(double[][] field, int width, int height, double[][] Gaussian) {
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
                    for (p2=0; p2<5; p2++) smoothedValue += field[x-2+p1][y-2+p2]; //Gaussian[p1][p2]*;
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
}


