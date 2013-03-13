package gov.nih.mipav.model.algorithms;

import gov.nih.mipav.model.file.FileBase;
import gov.nih.mipav.model.file.FileIO;
import gov.nih.mipav.model.file.FileInfoBase;
import gov.nih.mipav.model.file.FileInfoMATLAB;
import gov.nih.mipav.model.file.FileUtility;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.ModelLUT;
import gov.nih.mipav.model.structures.ModelStorageBase;
import gov.nih.mipav.model.structures.TransMatrix;
import gov.nih.mipav.model.structures.VOI;
import gov.nih.mipav.model.structures.VOIBaseVector;
import gov.nih.mipav.model.structures.VOIPoint;
import gov.nih.mipav.model.structures.VOIVector;
import gov.nih.mipav.model.structures.jama.GeneralizedInverse2;

import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.Preferences;
import gov.nih.mipav.view.ViewJFrameGraph;
import gov.nih.mipav.view.ViewJFrameImage;
import gov.nih.mipav.view.ViewUserInterface;

import java.awt.Color;
import java.awt.Font;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.Toolkit;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyAdapter;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.IOException;
import java.io.RandomAccessFile;
import java.util.Scanner;
import java.util.concurrent.ArrayBlockingQueue;
import java.util.zip.Inflater;

import javax.swing.BorderFactory;
import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTextField;
import javax.swing.border.EtchedBorder;
import javax.swing.border.TitledBorder;

import Jama.Matrix;
import Jama.SingularValueDecomposition;
import WildMagic.LibFoundation.Mathematics.Vector3f;



/**
 * 
 * @author William Gandler
 * This is a port of active shape model code from MATLAB to Java.   The license is given below:
    Copyright (c) 2010, Dirk-Jan Kroon
    All rights reserved.
    
    Redistribution and use in source and binary forms, with or without 
    modification, are permitted provided that the following conditions are 
    met:
    
        * Redistributions of source code must retain the above copyright 
          notice, this list of conditions and the following disclaimer.
        * Redistributions in binary form must reproduce the above copyright 
          notice, this list of conditions and the following disclaimer in 
          the documentation and/or other materials provided with the distribution
          
    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" 
    AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE 
    IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE 
    ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE 
    LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR 
    CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF 
    SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS 
    INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN 
    CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) 
    ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE 
    POSSIBILITY OF SUCH DAMAGE.

 
 *
 */
public class AlgorithmASM extends AlgorithmBase implements ActionListener {
    
//~ Instance fields ------------------------------------------------------------------------------------------------
    private String fileDir = "C:/Active Models/Fotos/";
    
    private boolean fatalError = false;
    
    private P p;
    
    private double Vertices[][];
    
    private int Lines[][];
    
    private JDialog pauseDialog = null;
    
    private ModelImage Itest = null;
    
    private VOI initialVOI = null;
    
    private JButton OKButton = null;
    
    private boolean pressedOK = false;
    
    private ViewJFrameImage testFrame = null;
    
    
  //~ Constructors ---------------------------------------------------------------------------------------------------

   public AlgorithmASM() {
       
   }
    
  //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Prepares this class for destruction.
     */
    public void finalize() {
        
        super.finalize();
    }
    
    public class Options {
        // Number of contour points interpolated between major landmarks
        public int ni;
        // Length of landmark intensity profile
        public int k;
        // Search length (in pixels) for optimal countour point position,
        // in both normal directions of the contour point.
        public int ns;
        // Number of image resolution scales
        public int nscales;
        // Set normal contour, limit to +- m*sqrt(eigenvalue)
        public int m;
        // Number of search iterations
        public int nsearch;
        // If verbose is true all debug images will be shown
        public boolean verbose;
        // The original minimal Mahalanobis distance using edge gradient (true)
        // or new minimal PCA parameters using the intensities. (false)
        public boolean originalsearch;
    }
    
    public class P {
        // Number of contour points clicked
        public short n;
        // The location of the contour points
        public double x[];
        public double y[];
        // Same length as the coordinates, with "0" a major landmark point
        // and "2" only a simple point on the contour
        public short t[];
        // The image
        public ModelImage I;
    }
    
    public class TData {
        public double offsetv[] = new double[2];
        public double offsetr;
        public double offsets;
    }
    
    public class ASMData {
        public double Vertices[][];
        public double CVertices[][];
        public double Normals[][];
        public double GrayProfiles[][];
        public double DerivativeGrayProfiles[][];
        public int Lines[][];
        public ModelImage I;
        public TData tform = new TData();
    }
    
    public class SData {
        public double x_mean[];
        public double Evectors[][];
        public double Evalues[];
        public double x[][];
        public int Lines[][];
    }
    
    public class LandData {
        public double S[][];
        public double Sinv[][];
        public double dg_mean[];
    }
    
    public class PData {
        public double Evectors[][];
        public double Evalues[];
        public double Emean[];
    }
    
    public class AppData {
        public LandData Landmarks[];
        public PData PCAData[];
    }

    public void ASM_2D_example() {
        int i;
        String is;
        String number;
        FileIO fileIO;
        ModelImage I;
        String fileName;
        boolean multiFile;
        FileInfoBase fileInfo;
        ASMData TrainingData[];
        AppData AppearanceData[];
        SData ShapeData = null;
        TData tform;
        double pos[][];
        Vector3f pointLoc;
        // This is an example of a working basic Active Shape Model, with a few hand pictures.
        
        // Literature used: Ginneken B. et al., "Active Shape Model Segmentation with Optimal Features",
        // IEEE Transactions on Medical Imaging, 2002.
        
        // Original MATLAB functions are written by D. Kroon University of Twente (February 2010)
        
        // Set options
        Options options = new Options();
        // Number of contour points interpolated between the major landmarks.
        options.ni = 20;
        // Length of landmark intensity profile
        options.k = 8;
        // Search length (in pixels) for optimal countour point position,
        // in both normal directions of the contour point.
        options.ns = 6;
        // Number of image resolution scales
        options.nscales = 2;
        // Set normal contour, limit to +- m*sqrt(eigenvalue)
        options.m = 3;
        // Number of search iterations
        //options.nsearch = 40;
        options.nsearch = 120;
        // If verbose if true, all debug images will be shown.
        options.verbose = false;
        // The original minimal Mahalanobis distance using edge gradient (true)
        // or new minimal PCA parameters using the intensities. (false)
        options.originalsearch = false;
        
        // Load training data
        // First load the hand training data sets (contour and image).
        // The LoadDataSetNiceContour routine, not only reads the contour points, but
        // also resamples them to get a nice uniform spacing, between the important
        // landmark contour points.
        TrainingData = new ASMData[10];
        multiFile = false;
        fileInfo = null;
        for (i = 1; i <= 10; i++) {
            is = String.valueOf(i); 
            number = "000";
            number = number.substring(0,number.length() - is.length()).concat(is);
            // All these .jpg images are ARGB
            fileName = "train".concat(number).concat(".jpg");
            fileIO = new FileIO();
            I = fileIO.readImage(fileName, fileDir, multiFile, fileInfo);
            fileName = "train".concat(number).concat(".mat");
            
            LoadDataSetNiceContour(fileName, options.ni, options.verbose);
            if (fatalError) {
                return;
            }
            TrainingData[i-1] = new ASMData();
            TrainingData[i-1].Vertices = Vertices;
            TrainingData[i-1].Lines = Lines;
            TrainingData[i-1].I = I;
            /*if (i == 1) {
                float xArr[] = new float[1];
                float yArr[] = new float[1];
                float zArr[] = new float[1];
                //for (int j = 0; j < Lines.length; j++) {
                for (int j = 0; j < Vertices.length; j++) {
                    VOI newPtVOI = new VOI((short) (j+1), String.valueOf(j+1), VOI.POINT, -1.0f);
                    newPtVOI.setColor(Color.RED);
                    xArr[0] = (float)Vertices[j][0];
                    yArr[0] = (float)Vertices[j][1];
                    //xArr[0] = (float)Vertices[Lines[j][0]][0];
                    //yArr[0] = (float)Vertices[Lines[j][0]][1];
                    zArr[0] = 0.0f;
                    newPtVOI.importCurve(xArr, yArr, zArr);
                    ((VOIPoint) (newPtVOI.getCurves().elementAt(0))).setFixed(true);
                    I.registerVOI(newPtVOI);
                }
                new ViewJFrameImage(I);
                return;
            } // if (i == 1)*/
        } // for (i = 1; i <= 10; i++)
        
        // Shape Model
        // Make the Shape model, which finds the variations between contours
        // in the training data sets.  And makes a PCA model describing normal contours.
        ShapeData = new SData();
        ASM_MakeShapeModel2D(TrainingData, ShapeData);
        
        boolean testme = false;
        if (testme) {
            double xtest[] = new double[ShapeData.x_mean.length];
            for (i = 0; i < Math.min(5, ShapeData.Evalues.length-1); i++) {
                for (int j = 0; j < ShapeData.x_mean.length; j++) {
                    xtest[j] = ShapeData.x_mean[j] + ShapeData.Evectors[j][i] * Math.sqrt(ShapeData.Evalues[i])*3.0;
                }
                float xArr[] = new float[1];
                float yArr[] = new float[1];
                float zArr[] = new float[1];
                for (int j = 0; j < xtest.length/2; j++) {
                    VOI newPtVOI = new VOI((short) (j+1), String.valueOf(j+1), VOI.POINT, -1.0f);
                    if (i == 0) {
                        newPtVOI.setColor(Color.RED);
                    }
                    else if (i == 1) {
                        newPtVOI.setColor(Color.GREEN);
                    }
                    else if (i == 2) {
                        newPtVOI.setColor(Color.BLUE);
                    }
                    else if (i == 3) {
                        newPtVOI.setColor(Color.CYAN);
                    }
                    else if (i == 4) {
                        newPtVOI.setColor(Color.WHITE);
                    }
                    else if (i == 5) {
                        newPtVOI.setColor(Color.MAGENTA);
                    }
                    xArr[0] = (float)xtest[j] + 400;
                    yArr[0] = (float)xtest[j+xtest.length/2] + 250;
                    zArr[0] = 0.0f;
                    newPtVOI.importCurve(xArr, yArr, zArr);
                    ((VOIPoint) (newPtVOI.getCurves().elementAt(0))).setFixed(true);
                    TrainingData[0].I.registerVOI(newPtVOI);    
                }
            }
            new ViewJFrameImage(TrainingData[0].I);
            return;
        }
        
        // Appearance model
        // Make the appearance model, which samples an intensity pixel profile/line
        // perpendicular to each contour point in each training data set. Which is
        // used to build correlation matrices for each landmark.  Which are used
        // in the optimization step, to find the best fit.
        AppearanceData = new AppData[options.nscales];
        ASM_MakeAppearanceModel2D(TrainingData, options, AppearanceData);
        
        // Test the ASM Model
        fileName = "test001.jpg";
        fileIO = new FileIO();
        // Image is ARGB.  Data should be converted to double.
        Itest = fileIO.readImage(fileName, fileDir, multiFile, fileInfo);
        testFrame = new ViewJFrameImage(Itest);
        
        // Initial position offset and rotation of the initial/mean contour
        tform = new TData();
    
        // tform.offsetv = [0 0];
        //tform.offsetr = -0.3;
        tform.offsetr = 0.0;
        //tform.offsets = 117;
        pos = new double[ShapeData.x_mean.length/2][2];
        for (i = 0; i < ShapeData.x_mean.length/2; i++) {
            pos[i][0] = ShapeData.x_mean[i];
            pos[i][1] = ShapeData.x_mean[ShapeData.x_mean.length/2 + i];
        }
        ASM_align_data_inverse2D(pos, tform);
        
        // Select the best starting position with the mouse
        // This is on the middle finger 1/4 from the base to the first joint
        // Figures 5 and 6 in A Brief Introduction to Statistical Shape Analysis by Mikkel B. Stegmann 
        // and David Delgado Gomez show the first point to be the intersection of the thumb and the wrist.
        createPauseDialog(this);

        while (!pressedOK) {

            try {
                sleep(5L);
            } catch (InterruptedException error) { }
        }
    
        pointLoc = initialVOI.exportPoint();
        tform.offsetv[0] = -pointLoc.X;
        tform.offsetv[1] = -pointLoc.Y;
        
        // Apply the ASM model on the test image
        ASM_ApplyModel2D(Itest, tform, ShapeData, AppearanceData, options);
    }
    
    private void ASM_ApplyModel2D(ModelImage Itest, TData tform, SData ShapeData, AppData AppearanceData[], Options options) {
        int n1;
        int i;
        double pos[][];
        int itt_res;
        double scale;
        PData PCAData[];
        int itt;
        double N[][];
        int n;
        int oXdim;
        int oYdim;
        float oXres;
        float oYres;
        TransMatrix xfrm;
        int interp = AlgorithmTransform.BILINEAR;
        int units[];
        AlgorithmTransform algoTrans;
        boolean doVOI = false;
        boolean doClip = true;
        boolean doPad = false;
        boolean doRotateCenter = false;
        Vector3f center = null;
        boolean doUpdateOrigin = true;
        float fillValue;
        ModelImage IScaledTest;
        int j;
        double gt[][];
        double dgt[][];
        int cf;
        double scaledPos[][];
        double f[][];
        double gi[];
        int k;
        int coffset;
        int coffset2;
        int m;
        double v[];
        double mid[];
        double bc[];
        double minVal;
        int iArr[];
        int movement[];
        double x_search[];
        double b[];
        double maxb;
        // Optimization
        
        // Show the test image if not shown
        if (testFrame == null) {
            testFrame = new ViewJFrameImage(Itest);
        }
        
        cf = 1;
        if (Itest.isColorImage()) {
            cf = 3;
        }
        

        // Total intensity line-profile needed, is the traingingLength + the
        // search length in both normal directions.
        n = options.k + options.ns;
        
        // The initial contour is the mean training data set contour
        n1 = ShapeData.x_mean.length/2;
        pos = new double[n1][2];
        for (i = 0; i < n1; i++) {
            pos[i][0] = ShapeData.x_mean[i];
            pos[i][1] = ShapeData.x_mean[i+n1];
        }
        
        // Position the initial contour at a location close to the correct location
        ASM_align_data_inverse2D(pos, tform);
        
        // Optimize from a rough to a fine image
        for (itt_res = options.nscales-1; itt_res >= 0; itt_res--) {
            // Scaling of the image  
            scale = Math.pow(2.0, -itt_res);
            
            PCAData = AppearanceData[itt_res].PCAData;
            
            // Get the intensity profiles of all landmarks and their first order
            // derivatives
            if (itt_res != 0) {
                oXdim = (int)Math.round(Itest.getExtents()[0] * scale);
                oYdim = (int)Math.round(Itest.getExtents()[1] * scale);
                oXres = Itest.getFileInfo(0).getResolutions()[0] * (Itest.getExtents()[0] - 1) / (oXdim - 1);
                oYres = Itest.getFileInfo(0).getResolutions()[1] * (Itest.getExtents()[1] - 1) / (oYdim - 1);
                xfrm = new TransMatrix(3);
                Itest.makeUnitsOfMeasureIdentical();
                units = new int[Itest.getUnitsOfMeasure().length];
                for (i = 0; i < units.length; i++) {
                    units[i] = Itest.getUnitsOfMeasure(i);
                }
                Itest.calcMinMax();
                fillValue = (float)Itest.getMin();
                algoTrans = new AlgorithmTransform(Itest, xfrm, interp, oXres, oYres, oXdim, oYdim, units, doVOI, doClip,
                        doPad, doRotateCenter, center);
                algoTrans.setFillValue(fillValue);
                algoTrans.setUpdateOriginFlag(doUpdateOrigin);
                algoTrans.run();
                IScaledTest = algoTrans.getTransformedImage();
                IScaledTest.calcMinMax();
                if (algoTrans != null) {
                    algoTrans.disposeLocal();
                    algoTrans = null;
                }
            } // if (itt_res != 0)
            else {
                IScaledTest = Itest;
            }
            
            // Do 50 ASM iterations
            for (itt = 1; itt <= options.nsearch; itt++) {
                // Plot the contour -leave out for now.  This would be 50 * 3 = 150 plots.
                
                // Calculate the normals of the contour points
                N = ASM_GetContourNormals2D(pos, ShapeData.Lines);
                
                // Create long intensity profiles on the contour normals, for search
                // of best point fit, using the correlation matrices made in the
                // appearance model. 
                scaledPos = new double[n1][2];
                for (i = 0; i < n1; i++) {
                    for (j = 0; j < 2; j++) {
                        scaledPos[i][j] = scale * pos[i][j];
                    }
                }
                gt = new double[(2*n+1)*cf][pos.length];
                dgt = new double[(2*n+1)*cf][pos.length];
                ASM_getProfileAndDerivatives2D(IScaledTest, scaledPos, N, n, gt, dgt);
                // Loop through all contour points
                f = new double[2*options.ns+1][n1];
                for (j = 0; j < n1; j++) {
                    // Search through the large sampled profile, for the optimal position
                    for (i = 0; i <= 2*options.ns; i++) {
                        // A profile from the large profile, with the length of the
                        // training profile (for rgb image 3x as long)
                        gi = new double[cf * (2*options.k+1)];
                        if (options.originalsearch) {
                            for (k = 0; k < cf; k++) {
                                coffset = k*(2*options.k+1);
                                coffset2 = k*(2*options.ns+1);
                                for (m = 0; m <= 2*options.k; m++) {
                                    gi[coffset+m] = dgt[i+coffset2+m][j];
                                } // for (m = 0; m <= 2*options.k; m++)
                            } // for (k = 0; k < cf; k++)
                            // Calculate the Mahalanobis distance from the current profile
                            // to the training data sets profiles through an inverse
                            // correlation matrix.
                            v = new double[cf*(2*options.k+1)];
                            for (k = 0; k < cf*(2*options.k+1); k++) {
                                v[k] = (gi[k] - AppearanceData[itt_res].Landmarks[j].dg_mean[k]);
                            } // for (k = 0; k < cf*(2*options.k+1); k++)
                            mid = new double[cf*(2*options.k+1)];
                            for (k = 0; k < cf*(2*options.k+1); k++) {
                                for (m = 0; m < cf*(2*options.k+1); m++) {
                                    mid[k] += v[m]*AppearanceData[itt_res].Landmarks[j].Sinv[m][k];
                                }
                            }
                            f[i][j] = 0.0;
                            for (k = 0; k < cf*(2*options.k+1); k++) {
                                f[i][j] += mid[k] * v[k];
                            }
                        } // if (options.originalsearch)
                        else {
                            for (k = 0; k < cf; k++) {
                                coffset = k*(2*options.k+1);
                                coffset2 = k*(2*options.ns+1);
                                for (m = 0; m <= 2*options.k; m++) {
                                    gi[coffset+m] = gt[i+coffset2+m][j];
                                } // for (m = 0; m <= 2*options.k; m++)
                            } // for (k = 0; k < cf; k++) 
                            // Calculate the PCA parameters, and normalize them with the variances
                            // (Seems to work better with color images than the original method)
                            bc = new double[PCAData[j].Evectors[0].length];
                            for (k = 0; k < bc.length; k++) {
                                for (m = 0; m < cf*(2*options.k+1); m++) {
                                    bc[k] += PCAData[j].Evectors[m][k] *(gi[m] - PCAData[j].Emean[m]);
                                }
                            }
                            f[i][j] = 0.0;
                            for (k = 0; k < bc.length; k++) {
                                bc[k] = bc[k]/(Math.sqrt(PCAData[j].Evalues[k]));
                                f[i][j] += (bc[k]*bc[k]);                            
                            }
                        } // else !options.originalsearch
                    } // for (i = 0; i <= 2*options.ns; i++)
                } // for (j = 0; j < n1; j++)
                // Find the lowest Mahalanobis distance, and store it as movement step
                iArr = new int[n1];
                for (j = 0; j < n1; j++) {
                    minVal = Double.MAX_VALUE;
                    iArr[j] = -1;
                    for (i = 0; i <= 2*options.ns; i++) {
                        if (f[i][j] < minVal) {
                            minVal = f[i][j];
                            iArr[j] = i;
                        }
                    }
                } // for (j = 0; j < n1; j++)
                movement = new int[n1];
                for (j = 0; j < n1; j++) {
                    movement[j] = iArr[j] - options.ns;
                }
                
                // Move the points to their new optimal positions
                for (j = 0; j < n1; j++) {
                    pos[j][0] = pos[j][0] + (1.0/scale)*movement[j]*N[j][0];
                    pos[j][1] = pos[j][1] + (1.0/scale)*movement[j]*N[j][1];
                }
                
                // Show new positions
                
                // Remove translation and rotation, as done when training the model
                ASM_align_data2D(pos, tform);
                
                // Describe the model by a vector b with model parameters
                x_search = new double[2*n1];
                for (j = 0; j < n1; j++) {
                    x_search[j] = pos[j][0];
                    x_search[j+n1] = pos[j][1];
                }
                b = new double[ShapeData.Evectors[0].length];
                for (j = 0; j < ShapeData.Evectors[0].length; j++) {
                    for (i = 0; i < 2*n1; i++) {
                        b[j] += (ShapeData.Evectors[i][j] * (x_search[i] - ShapeData.x_mean[i]));
                    }
                }
                
                // Limit the model parameters based on what is considered a normal
                // contour, using the eigenvalues of the PCA model
                for (j = 0; j < b.length; j++) {
                    maxb = options.m * Math.sqrt(ShapeData.Evalues[j]);
                    b[j] = Math.max(Math.min(b[j],maxb), -maxb);
                }
                
                // Transform the model parameter vector b back to contour positions
                mid = new double[2*n1];
                for (j = 0; j < 2*n1; j++) {
                    for (i = 0; i < b.length; i++) {
                        mid[j] += ShapeData.Evectors[j][i]*b[i];    
                    }
                    x_search[j] = ShapeData.x_mean[j] + mid[j];
                }
                
                for (j = 0; j < n1; j++) {
                    pos[j][0] = x_search[j];
                    pos[j][1] = x_search[j+n1];
                }
                
                // Now add the previously removed translation and rotation
                ASM_align_data_inverse2D(pos, tform);
            } // for (itt = 1; itt <= options.nsearch; itt++)
            if (itt_res != 0) {
                IScaledTest.disposeLocal();
                IScaledTest = null;
            }
        } // for (itt_res = options.nscales-1; itt_res >= 0; itt_res--)
        
        float xArr[] = new float[1];
        float yArr[] = new float[1];
        float zArr[] = new float[1];
        for (j = 0; j < pos.length; j++) {
            VOI newPtVOI = new VOI((short) (j+1), String.valueOf(j+1), VOI.POINT, -1.0f);
            newPtVOI.setColor(Color.RED);
            xArr[0] = (float)pos[j][0];
            yArr[0] = (float)pos[j][1];
            zArr[0] = 0.0f;
            newPtVOI.importCurve(xArr, yArr, zArr);
            ((VOIPoint) (newPtVOI.getCurves().elementAt(0))).setFixed(true);
            Itest.registerVOI(newPtVOI);
        }
    }
    
    private void createPauseDialog(ActionListener al) {
        JPanel panel;
        TitledBorder border;
        Font serif12, serif12B;
        JLabel label1;
        JLabel label2;

        pauseDialog = new JDialog(ViewUserInterface.getReference().getActiveImageFrame(), "Press OK to continue", false);
        pauseDialog.setLocation((Toolkit.getDefaultToolkit().getScreenSize().width / 2) -
                                  (pauseDialog.getBounds().width / 2),
                                  (Toolkit.getDefaultToolkit().getScreenSize().height / 2) -
                                  (pauseDialog.getBounds().height / 2));
        pauseDialog.getContentPane().setLayout(new GridBagLayout());

        pauseDialog.setSize(300, 160);

        serif12 = MipavUtil.font12;
        serif12B = MipavUtil.font12B;

        panel = new JPanel();
        panel.setBorder(BorderFactory.createEmptyBorder(3, 3, 3, 3));
        panel.setLayout(new GridBagLayout());

        GridBagConstraints gbc = new GridBagConstraints();
        gbc.gridwidth = 1;
        gbc.gridheight = 1;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.weightx = 1;
        gbc.insets = new Insets(3, 3, 3, 3);
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.gridx = 0;
        gbc.gridy = 0;
        panel.setForeground(Color.black);
        border = new TitledBorder("Instructions");
        border.setTitleColor(Color.black);
        border.setBorder(new EtchedBorder());
        border.setTitleFont(serif12B);
        panel.setBorder(border);
        pauseDialog.getContentPane().add(panel, gbc);

        gbc.gridx = 0;
        gbc.gridy = 0;
        label1 = new JLabel("Select the best starting postion with a point VOI");
        label1.setForeground(Color.black);
        label1.setFont(serif12);
        panel.add(label1, gbc);
        
        gbc.gridx = 0;
        gbc.gridy = 1;
        label2 = new JLabel("On the middle finger 1/4 from the base to the first joint");
        label2.setForeground(Color.black);
        label2.setFont(serif12);
        panel.add(label2, gbc);

        JPanel buttonPanel = new JPanel();
        OKButton = new JButton("OK");
        OKButton.setMinimumSize(MipavUtil.defaultButtonSize);
        OKButton.setPreferredSize(MipavUtil.defaultButtonSize);
        OKButton.setFont(serif12B);
        OKButton.addActionListener(al);
        buttonPanel.add(OKButton);

        gbc.gridx = 0;
        gbc.gridy = 1;
        pauseDialog.getContentPane().add(buttonPanel, gbc);
        pauseDialog.setResizable(true);
        pauseDialog.setVisible(true);

    }

    // ************************************************************************
    // **************************** Action Events *****************************
    // ************************************************************************

    /**
     * Calls various methods depending on the action.
     *
     * @param  event  event that triggered function
     */
    public void actionPerformed(ActionEvent event) {
        int nPoints;
        VOIVector voiVector;
        VOIBaseVector curves;
        int i;
        Object source = event.getSource();

        if (source == OKButton) {
            nPoints = 0;
            voiVector = Itest.getVOIs();
            for (i = 0; i < voiVector.size(); i++) {
                initialVOI = voiVector.VOIAt(i);
                if (initialVOI.getCurveType() == VOI.POINT) {
                    curves = initialVOI.getCurves();
                    nPoints += curves.size();
                }
            }
            if (nPoints == 0) {
                MipavUtil.displayError("No points were present");
                return;
            }
            else if (nPoints > 1) {
                MipavUtil.displayError(nPoints + " points were selected instead of only 1");
                return;
            } 
            else {
                pauseDialog.dispose();
                pressedOK = true;
            }
        }
    }
    
    private void ASM_align_data_inverse2D(double pos[][], TData tform) {
        int i;
        int len;
        double rot;
        double dist;
        // Correct for rotation  
        len = pos.length;
        for (i = 0; i < len; i++) {
            rot = Math.atan2(pos[i][1], pos[i][0]);
            rot = rot - tform.offsetr;
            dist = Math.sqrt(pos[i][0]*pos[i][0] + pos[i][1]*pos[i][1]);
            // tform.offsets
            pos[i][0] = dist*Math.cos(rot);
            pos[i][1] = dist*Math.sin(rot);
            pos[i][0] = pos[i][0] - tform.offsetv[0];
            pos[i][1] = pos[i][1] - tform.offsetv[1];
        } // for (i = 0; i < len; i++)
    }
    
    // Gray-Level Appearance Model
    private void ASM_MakeAppearanceModel2D(ASMData[] TrainingData, Options options, AppData AppearanceData[]) {
        int s;
        int n1;
        int i;
        int itt_res;
        double scale;
        double P[][] = new double[TrainingData[0].Vertices.length][2];
        int j;
        int k;
        int m;
        int oXdim;
        int oYdim;
        float oXres;
        float oYres;
        TransMatrix xfrm;
        int interp = AlgorithmTransform.BILINEAR;
        int units[];
        AlgorithmTransform algoTrans;
        boolean doVOI = false;
        boolean doClip = true;
        boolean doPad = false;
        boolean doRotateCenter = false;
        Vector3f center = null;
        boolean doUpdateOrigin = true;
        float fillValue;
        ModelImage Ismall;
        double N[][];
        int cf = 1;
        int p1;
        double dg[][];
        double dg_mean[];
        Matrix dgMat;
        Matrix dgtMat;
        Matrix sMat;
        double g[][];
        double Evalues[];
        double Evectors[][];
        double Emean[];
        double totSum;
        double sum;
        double Evectors2[][];
        double Evalues2[];
        GeneralizedInverse2 ge;
        
        // Number of TrainingData sets
        s = TrainingData.length;
        
        // Number of landmarks
        n1 = TrainingData[0].Vertices.length;
        
        // Profile length * space for rgb
        cf = 1;
        if (TrainingData[0].I.isColorImage()) {
            cf = 3;
        }
        p1 = cf * (2*options.k + 1);
        
        // Calculate the normals of the contours of all training data
        for (i = 0; i < s; i++) {
            TrainingData[i].Normals = ASM_GetContourNormals2D(TrainingData[i].Vertices, TrainingData[i].Lines); 
            /*for (j = 0; j < TrainingData[i].Normals.length; j++) {
                Preferences.debug("x = " + TrainingData[i].Normals[j][0] + " y = " + TrainingData[i].Normals[j][1] + "\n");
            }*/
        }
        
        
        
        // Inverse of covariance matrix sometimes badly scaled
        // Don't warn of this
        
        // Get the landmark profiles for 2 image scales (for multiscale ASM)
        for (itt_res = 0; itt_res < options.nscales; itt_res++) {
            AppearanceData[itt_res] = new AppData();
            AppearanceData[itt_res].Landmarks = new LandData[n1];
            PData PCAData[] = new PData[n1];
            scale = Math.pow(2.0, -itt_res);
            
            // Get the pixel profiles of every landmark perpendicular to the contour
            for (i = 0; i < s; i++) {
                for (j = 0; j < TrainingData[i].Vertices.length; j++) {
                    for (k = 0; k < 2; k++) {
                        P[j][k] = (TrainingData[i].Vertices[j][k] - 0.5)*scale + 0.5;
                    }
                }
                if (itt_res != 0) {
                    oXdim = (int)Math.round(TrainingData[i].I.getExtents()[0] * scale);
                    oYdim = (int)Math.round(TrainingData[i].I.getExtents()[1] * scale);
                    oXres = TrainingData[i].I.getFileInfo(0).getResolutions()[0] * (TrainingData[i].I.getExtents()[0] - 1) / (oXdim - 1);
                    oYres = TrainingData[i].I.getFileInfo(0).getResolutions()[1] * (TrainingData[i].I.getExtents()[1] - 1) / (oYdim - 1);
                    xfrm = new TransMatrix(3);
                    TrainingData[i].I.makeUnitsOfMeasureIdentical();
                    units = new int[TrainingData[i].I.getUnitsOfMeasure().length];
                    for (j = 0; j < units.length; j++) {
                        units[j] = TrainingData[i].I.getUnitsOfMeasure(j);
                    }
                    TrainingData[i].I.calcMinMax();
                    fillValue = (float)TrainingData[i].I.getMin();
                    algoTrans = new AlgorithmTransform(TrainingData[i].I, xfrm, interp, oXres, oYres, oXdim, oYdim, units, doVOI, doClip,
                            doPad, doRotateCenter, center);
                    algoTrans.setFillValue(fillValue);
                    algoTrans.setUpdateOriginFlag(doUpdateOrigin);
                    algoTrans.run();
                    Ismall = algoTrans.getTransformedImage();
                    Ismall.calcMinMax();
                    if (algoTrans != null) {
                        algoTrans.disposeLocal();
                        algoTrans = null;
                    }
                }
                else {
                    Ismall = TrainingData[i].I;
                } // if (itt_res != 0)
                /*if (i == 0) {
                    float xArr[] = new float[1];
                    float yArr[] = new float[1];
                    float zArr[] = new float[1];
                    for (j = 0; j < P.length; j++) {
                        VOI newPtVOI = new VOI((short) (j+1), String.valueOf(j+1), VOI.POINT, -1.0f);
                        newPtVOI.setColor(Color.RED);
                        xArr[0] = (float)P[j][0];
                        yArr[0] = (float)P[j][1];
                        zArr[0] = 0.0f;
                        newPtVOI.importCurve(xArr, yArr, zArr);
                        ((VOIPoint) (newPtVOI.getCurves().elementAt(0))).setFixed(true);
                        Ismall.registerVOI(newPtVOI);
                    }
                    new ViewJFrameImage(Ismall);
                }*/
                N = TrainingData[i].Normals;
                cf = 1;
                if (TrainingData[i].I.isColorImage()) {
                    cf = 3;    
                }
                TrainingData[i].GrayProfiles = new double[(2*options.k+1)*cf][P.length];
                TrainingData[i].DerivativeGrayProfiles = new double[(2*options.k+1)*cf][P.length];
                ASM_getProfileAndDerivatives2D(Ismall, P, N, options.k, TrainingData[i].GrayProfiles, TrainingData[i].DerivativeGrayProfiles);
            } // for (i = 0; i < s; i++)
            
            /*boolean testme = true;
            if (testme) {
                for (i = 0; i < s; i++) {
                    if (cf == 1) {
                        float mean[] = new float[(2*options.k+1)];
                        for (j = 0; j < (2*options.k+1); j++) {
                            for (m = 0; m < P.length; m++) {
                                mean[j] += TrainingData[i].GrayProfiles[j][m];
                            }
                            mean[j] = mean[j]/P.length;
                        }
                        
                        float xax[] = new float[(2*options.k+1)];
                        for (j = 0; j < 2*options.k+1; j++) {
                            xax[j] = j;
                        }
                        new ViewJFrameGraph(xax, mean, "Mean intensity profile " + (i+1), "", "", Color.BLUE);
                    } //if (cf == 1)
                    else {
                        float meanr[] = new float[(2*options.k+1)];
                        for (j = 0; j < (2*options.k+1); j++) {
                            for (m = 0; m < P.length; m++) {
                                meanr[j] += TrainingData[i].GrayProfiles[j][m];
                            }
                            meanr[j] = meanr[j]/P.length;
                        }
                        
                        float xaxr[] = new float[(2*options.k+1)];
                        for (j = 0; j < 2*options.k+1; j++) {
                            xaxr[j] = j;
                        }
                        new ViewJFrameGraph(xaxr, meanr, "Red mean intensity profile "  + (i+1), "", "", Color.RED);
                        
                        float meang[] = new float[(2*options.k+1)];
                        for (j = 0; j < (2*options.k+1); j++) {
                            for (m = 0; m < P.length; m++) {
                                meang[j] += TrainingData[i].GrayProfiles[(2*options.k+1)+j][m];
                            }
                            meang[j] = meang[j]/P.length;
                        }
                        
                        float xaxg[] = new float[(2*options.k+1)];
                        for (j = 0; j < 2*options.k+1; j++) {
                            xaxg[j] = j;
                        }
                        new ViewJFrameGraph(xaxg, meang, "Green mean intensity profile " + (i+1), "", "", Color.GREEN);
                        
                        float meanb[] = new float[(2*options.k+1)];
                        for (j = 0; j < (2*options.k+1); j++) {
                            for (m = 0; m < P.length; m++) {
                                meanb[j] += TrainingData[i].GrayProfiles[2*(2*options.k+1)+j][m];
                            }
                            meanb[j] = meanb[j]/P.length;
                        }
                        
                        float xaxb[] = new float[(2*options.k+1)];
                        for (j = 0; j < 2*options.k+1; j++) {
                            xaxb[j] = j;
                        }
                        new ViewJFrameGraph(xaxb, meanb, "Blue mean intensity profile " + (i+1), "", "", Color.BLUE);
                        
                    }
                }
            } // if (testme)*/
            
           
            
            // Calculate a covariance matrix for all landmarks
            for (j = 0; j < n1; j++) {
                AppearanceData[itt_res].Landmarks[j] = new LandData();
                PCAData[j] = new PData();
                // The original search method using Mahalanobis distance with edge gradient information 
                dg = new double[p1][s];
                for (m = 0; m < p1; m++) {
                    for (i = 0; i < s; i++) {
                        dg[m][i] = TrainingData[i].DerivativeGrayProfiles[m][j];
                    } // for (i = 0; i < s; i++)
                } // for (m = 0; m < 2*options.k+1; m++)
                dg_mean = new double[p1];
                for (m = 0; m < p1; m++) {
                    for (i = 0; i < s; i++) {
                        dg_mean[m] += dg[m][i];
                    }
                    dg_mean[m] /= s;
                } // for (m = 0; m < p1; m++)
                for (m = 0; m < p1; m++) {
                    for (i = 0; i < s; i++) {
                        dg[m][i] = dg[m][i] - dg_mean[m];
                    }
                } // for (m = 0; m < p1; m++)
                // Calculate the covariance matrix and its inverse
                // First index is row and second is column
                // The covariance:
                // 1.) removes the mean from each column
                // 2.) Multiplies transpose times original
                // 3.) Divide by number of rows - 1 in original
                // dg' has s rows and p1 columns
                // Have already subtracted out mean of s rows from each column
                dgMat = new Matrix(dg);
                dgtMat = dgMat.transpose();
                sMat = dgMat.times(dgtMat).times(1.0/(s - 1.0));
                AppearanceData[itt_res].Landmarks[j].S = sMat.getArray();
                ge = new GeneralizedInverse2(AppearanceData[itt_res].Landmarks[j].S, p1, p1);
                AppearanceData[itt_res].Landmarks[j].Sinv = ge.pinv();
                //AppearanceData[itt_res].Landmarks[j].Sinv = sMat.inverse().getArray();
                AppearanceData[itt_res].Landmarks[j].dg_mean = dg_mean;
                
                // The new search method using PCA on intensities, and minimizing
                // parameters/ the distance to the mean during the search.
                // Make a matrix with all intensities
                g = new double[p1][s];
                for (m = 0; m < p1; m++) {
                    for (i = 0; i < s; i++) {
                        g[m][i] = TrainingData[i].GrayProfiles[m][j];
                    }
                } // for (m = 0; m < p1; m++)
                Evalues = new double[g[0].length];
                Evectors = new double[g.length][Math.min(g.length+1, g[0].length)];
                Emean = new double[g.length];
                PCA(g, Evalues, Evectors, Emean);
                // Keep only 98% of all eigenvectors, (remove contour noise)
                totSum = 0.0;
                for (i = 0; i < Evalues.length; i++) {
                    totSum += Evalues[i];
                }
                sum = 0.0;
                for (i = 0; i < Evalues.length; i++) {
                    sum += Evalues[i];
                    if (sum > 0.98 * totSum) {
                        break;
                    }
                }
                Evectors2 = new double[g.length][i+1];
                Evalues2 = new double[i+1];
                for (m = 0; m < g.length; m++) {
                    for (k = 0; k <= i; k++) {
                        Evectors2[m][k] = Evectors[m][k];   
                    }
                }
                for (m = 0; m <= i; m++) {
                    Evalues2[m] = Evalues[m];
                }
                PCAData[j].Evectors = Evectors2;
                PCAData[j].Evalues = Evalues2;
                PCAData[j].Emean = Emean;
            } // for (j = 0; j < n1; j++)
            AppearanceData[itt_res].PCAData = PCAData;
        } // for (itt_res = 0; itt_res < options.nscales; itt_res++)
    }
    
    private void ASM_getProfileAndDerivatives2D(ModelImage I, double[][] P, double[][] N, int k,
            double[][] gtc, double[][] dgtc) {
        // This function getProfileAndDerivatives samples the image on lines perpendicular to the contour points,
        // which are used to train the appearance model, and to find the best location for a contour point later on.
        
        // inputs,
        // I: The image color or greyscale
        // P: The locations of the contour points
        // N: The normals of the contour points
        // k: The length of the sampled line in one normal direction,
        //    total length becomes 2*k+1
        
        // outputs,
        // gtc: Columns with the sampled lines perpendicular to the contour
        // dgtc: The first order derivatives of the sampled lines
        
        // Original function written by D. Kroon University of Twente (February 2010)
        
        int cf;
        int i;
        double d1[] = new double[P.length];
        double d2[] = new double[P.length];
        double xi[][] = new double[P.length][2*k+1];
        double yi[][] = new double[P.length][2*k+1];
        double gt[][] = new double[2*k+1][P.length];
        double dgt[][] = new double[2*k+1][P.length];
        int j;
        int m;
        int sliceSize = I.getExtents()[0] * I.getExtents()[1];
        double buffer[] = new double[sliceSize];
        int xfl;
        int yfl;
        int xce;
        int yce;
        int b;
        double eps = Math.pow(2.0,-52);
        double ngt[] = new double[P.length];
        
        cf = 1;
        if (I.isColorImage()) {
            cf = 3;
        }
        for (i = 0; i < cf; i++) {
            if (I.isColorImage()) {
                try {
                    I.exportRGBDataNoLock((i+1), 0, sliceSize, buffer);
                }
                catch (IOException ex) {
                    System.err.println("IOException " + ex + " on I.exportRGBDataNoLock((" + (i+1) + "), 0, sliceSize, buffer)");
                    return;
                }
            }
            else {
                try {
                    I.exportData(0, sliceSize, buffer);
                }
                catch (IOException ex) {
                    System.err.println("IOException " + ex + " on I.exportData(0, sliceSize, buffer)");
                    return;
                }
            }
            for (j = 0; j < P.length; j++) {
                d1[j] = (P[j][0] - N[j][0]*k);
                d2[j] = (P[j][0] + N[j][0]*k);
            }
            linspace_multi(d1, d2, 2*k+1, xi);
            for (j = 0; j < P.length; j++) {
                d1[j] = (P[j][1] - N[j][1]*k);
                d2[j] = (P[j][1] + N[j][1]*k);
            }
            linspace_multi(d1, d2, 2*k+1, yi);
            for (j = 0; j < P.length; j++) {
                for (m = 0; m < 2*k+1; m++) {
                    if (xi[j][m]  < 0.0) {
                        xi[j][m] = 0.0;
                    }
                    if (xi[j][m] > I.getExtents()[0]-1) {
                        xi[j][m] = I.getExtents()[0]-1;
                    }
                    if (yi[j][m] < 0.0) {
                        yi[j][m] = 0.0;
                    }
                    if (yi[j][m] > I.getExtents()[1]-1) {
                        yi[j][m] = I.getExtents()[1]-1;
                    }
                    xfl = (int)Math.floor(xi[j][m]);
                    yfl = (int)Math.floor(yi[j][m]);
                    xce = xfl + 1;
                    yce = yfl + 1;
                    gt[m][j] = buffer[xfl + I.getExtents()[0]*yfl]*(yce - yi[j][m])*(xce - xi[j][m]);
                    if (xfl < I.getExtents()[0] - 1) {
                        gt[m][j] += buffer[xce + I.getExtents()[0]*yfl]*(yce - yi[j][m])*(xi[j][m] - xfl);
                    }
                    if (yfl < I.getExtents()[1] - 1) {
                        gt[m][j] += buffer[xfl + I.getExtents()[0]*yce]*(yi[j][m] - yfl)*(xce - xi[j][m]);
                    }
                    if ((xfl < I.getExtents()[0] - 1) && (yfl < I.getExtents()[1] - 1)) {
                        gt[m][j] += buffer[xce + I.getExtents()[0]*yce]*(yi[j][m] - yfl)*(xi[j][m] - xfl);
                    }
                    if (Double.valueOf(gt[m][j]).isNaN()) {
                        gt[m][j] = 0.0;
                    }
                } // for (m = 0; m < P.length; m++)
            } // for (j = 0; j < 2*k+1; j++)
            // Get the derivatives
            for (m = 0; m < P.length; m++) {
                dgt[0][m] = gt[1][m] - gt[0][m];
            }
            for (j = 1; j < 2*k; j++) {
                for (m = 0; m < P.length; m++) {
                    dgt[j][m] = (gt[j+1][m] - gt[j-1][m])/2.0;
                }
            }
            for (m = 0; m < P.length; m++) {
                dgt[2*k][m] = gt[2*k][m] - gt[2*k-1][m];
            }
            // Store the grey profiles and derivatives for the different color channels
            b = i*(2*k+1);
            for (j = 0; j < 2*k+1; j++) {
                for (m = 0; m < P.length; m++) {
                    gtc[b+j][m] = gt[j][m];
                    dgtc[b+j][m] = dgt[j][m];
                }
            }
        } // for (i = 0; i < cf; i++)
        // Normalize the derivatives
        for (m = 0; m < P.length; m++) {
            for (j = 0; j < cf*(2*k+1); j++) {
                ngt[m] += Math.abs(dgtc[j][m]);
            }
            ngt[m] += eps;
            for (j = 0; j < cf*(2*k+1); j++) {
                dgtc[j][m] = dgtc[j][m]/ngt[m];
            }
        }
    }
    
    private void linspace_multi(double d1[], double d2[], int n, double X[][]) {
      double A[][] = new double[d1.length][n-1];
      double B[][] = new double[d1.length][n-1];
      int i;
      int j;
      
      for (i = 0; i < d1.length; i++) {
          for (j = 0; j <= n-2; j++) {
              A[i][j] = j;
          }
      }
      
      for (i = 0; i < d1.length; i++) {
          for (j = 0; j <= n-2; j++) {
              B[i][j] = (d2[i] - d1[i]);
          }
      }
      
      for (i = 0; i < d1.length; i++) {
          for (j = 0; j <= n-2; j++) {
              X[i][j] = A[i][j] * B[i][j]/(n - 1.0);
              X[i][j] += d1[i];
          }
          X[i][n-1] = d2[i];
      }
    }
    
    private double[][] ASM_GetContourNormals2D(double[][] V, int[][] L) {
        // This function calculates the normals, of the contour points
        // using the neighboring points of each contour point, and
        // forward and backward differences on the end points
        
        // inputs,
        // V: List of Vertices 2 x N
        // L: Line list, with indices to the vertices 2 x M
        
        // outputs,
        // N: The normals of the Vertices
        
        double DT[][] = new double[V.length][V[0].length];
        double D1[][] = new double[V.length][V[0].length];
        double D2[][] = new double[V.length][V[0].length];
        double D[][] = new double[V.length][V[0].length];
        double LL[] = new double[V.length];
        double N[][] = new double[V.length][V[0].length];
        int i;
        int j;
        
        // Derivatives of the contour
        for (i = 0; i < V.length; i++) {
            for (j = 0; j < V[0].length; j++) {
                DT[i][j] = V[L[i][0]][j] - V[L[i][1]][j];
            }
        }
        for (i = 0; i < V.length; i++) {
            for (j = 0; j < V[0].length; j++) {
                D1[L[i][0]][j] = DT[i][j];
                D2[L[i][1]][j] = DT[i][j];
            }
        }
        for (i = 0; i < V.length; i++) {
            for (j = 0; j < V[0].length; j++) {
                D[i][j] = D1[i][j] + D2[i][j];
            }
        }
        for (i = 0; i < V.length; i++) {
            LL[i] = Math.sqrt(D[i][0]*D[i][0] + D[i][1]*D[i][1]);
        }
        for (i = 0; i < V.length; i++) {
            N[i][0] = -D[i][1]/LL[i];
            N[i][1] = D[i][0]/LL[i];
        }
        return N;
    }
    
    private void ASM_MakeShapeModel2D(ASMData[] TrainingData, SData ShapeData) {
        int s;
        int n1;
        int i;
        int j;
        int k;
        double x[][];
        double Evalues[] = null;
        double Evectors[][] = null;
        double x_mean[] = null;
        double totSum;
        double sum;
        double Evectors2[][];
        double Evalues2[];
        
        // Number of datasets
        s = TrainingData.length;
        
        // Number of landmarks
        n1 = TrainingData[0].Vertices.length;
        
        // Shape model
        
        // Remove rotation and translation
        // (Procrustes analysis would also be possible, see AAM_align_data)
        for (i = 0; i < s; i++) {
            TrainingData[i].CVertices = new double[TrainingData[i].Vertices.length][TrainingData[i].Vertices[0].length];
            for (j = 0; j < TrainingData[i].Vertices.length; j++) {
                for (k = 0; k < TrainingData[i].Vertices[0].length; k++) {
                    TrainingData[i].CVertices[j][k] = TrainingData[i].Vertices[j][k];
                }
            }
            ASM_align_data2D(TrainingData[i].CVertices, TrainingData[i].tform);
        }
        
        // Construct a matrix with all contour point data of the training data set
        x = new double[2*n1][s];
        for (i = 0; i < s; i++) {
            for (j = 0; j < n1; j++) {
                x[j][i] = TrainingData[i].CVertices[j][0];
                x[j+n1][i] = TrainingData[i].CVertices[j][1];
            }
        }
        
        Evalues = new double[x[0].length];
        Evectors = new double[x.length][Math.min(x.length+1, x[0].length)];
        x_mean = new double[x.length];
        PCA(x, Evalues, Evectors, x_mean);
        
        // Keep only 98% of all eigenvectors, (remove contour noise)
        totSum = 0.0;
        for (i = 0; i < Evalues.length; i++) {
            totSum += Evalues[i];
        }
        sum = 0.0;
        for (i = 0; i < Evalues.length; i++) {
            sum += Evalues[i];
            if (sum > 0.98 * totSum) {
                break;
            }
        }
        Evectors2 = new double[x.length][i+1];
        Evalues2 = new double[i+1];
        for (j = 0; j < x.length; j++) {
            for (k = 0; k <= i; k++) {
                Evectors2[j][k] = Evectors[j][k];   
            }
        }
        for (j = 0; j <= i; j++) {
            Evalues2[j] = Evalues[j];
        }
        
        // Store the eigenvectors and eigenvalues
        ShapeData.Evectors = Evectors2;
        ShapeData.Evalues = Evalues2;
        ShapeData.x_mean = x_mean;
        ShapeData.x = x;
        ShapeData.Lines = TrainingData[0].Lines;
        
    }
    
    private void PCA(double x[][], double Evalues[], double Evectors[][], double x_mean[]) {
        // PCA using Single Value Decomposition
        // Obtaining mean vector, eigenvectors, and eigenvalues
        
        // inputs:
        // x : M by N matrix with M the training vector length and N the number of training
        // data sets
        
        // outputs:
        // Evalues: The eigenvalues of the data
        // Evectors: The eigenvectors of the data
        // x_mean: The mean training data
        
        int s;
        int i;
        int j;
        double x2[][] = new double[x.length][x[0].length];
        double sq;
        Matrix xMat;
        SingularValueDecomposition svd;
        double singularValues[];
        
        Matrix matU;
        double U2[][];
        
        s = x[0].length;
        // Calculate the mean
        for (i = 0; i < x.length; i++) {
            for (j = 0; j < s; j++) {
                x_mean[i] += x[i][j];
            }
            x_mean[i] = x_mean[i]/s;
        }
        
        // Subtract the mean
        sq = Math.sqrt(s - 1.0);
        for (i = 0; i < x.length; i++) {
            for (j = 0; j < s; j++) { 
                x2[i][j] = (x[i][j] - x_mean[i])/sq;    
            }
        }
        
        // Do the SVD
        xMat = new Matrix(x2);
        svd = new SingularValueDecomposition(xMat);
        singularValues = svd.getSingularValues();
        for (i = 0; i < s; i++) {
            Evalues[i] = singularValues[i] * singularValues[i];
        }
        matU = svd.getU();
        U2 = matU.getArray();
        for (i = 0; i < x.length; i++) {
            for (j = 0; j < U2[0].length; j++) {
                Evectors[i][j]= U2[i][j] * Math.signum(U2[0][j]);
            }
        }
       
    }
    
    private void ASM_align_data2D(double Vertices[][], TData tform) {
        double totalv0;
        double totalv1;
        int i;
        double offsetv[] = new double[2];
        double rot[] = new double[Vertices.length];
        double totr;
        double offsetr;
        double dist[] = new double[Vertices.length];
        // Aligns the contours positions, center the data and remove rotation
        
        // Center the data to remove translation
        totalv0 = 0.0;
        totalv1 = 0.0;
        for (i = 0; i < Vertices.length; i++) {
            totalv0 += Vertices[i][0];
            totalv1 += Vertices[i][1];
        }
        offsetv[0] = -totalv0/Vertices.length;
        offsetv[1] = -totalv1/Vertices.length;
        
        for (i = 0; i < Vertices.length; i++) {
            Vertices[i][0] += offsetv[0];
            Vertices[i][1] += offsetv[1];
        }
        
        // Correct for rotation
        // Calculate angle to center of all points
        for (i = 0; i < Vertices.length; i++) {
            rot[i] = Math.atan2(Vertices[i][1], Vertices[i][0]);
        }
        // Subtract the mean angle
        totr = 0.0;
        for (i = 0; i < rot.length; i++) {
            totr += rot[i];    
        }
        offsetr = -totr/rot.length;
        for (i = 0; i < Vertices.length; i++) {
            rot[i] += offsetr;
        }
        // Make the new points, which all have the same rotation
        for (i = 0; i < Vertices.length; i++) {
            dist[i] = Math.sqrt(Vertices[i][0]*Vertices[i][0] + Vertices[i][1]*Vertices[i][1]);
            Vertices[i][0] = dist[i] * Math.cos(rot[i]);
            Vertices[i][1] = dist[i] * Math.sin(rot[i]);
        }
        
        // Store transformation object
        tform.offsetv[0] = offsetv[0];
        tform.offsetv[1] = offsetv[1];
        tform.offsetr = offsetr;
    }
    
    private void LoadDataSetNiceContour(String fileName, int nBetween, boolean verbose) {
        // Load the contour points and a photo of a dataset
        // The routine will interpolate a number of evenly spaced contour points between
        // the points marked as major landmark points.
        
        // The dataset .mat file must contain a structure "p"
        // p.n : Field 0 Number of contour points clicked
        // p.x Field 1, p.y Field 2: The location of the contour points
        // p.t Field 3: Same length as the coordinates, with "0" a major landmark point
        //       and "2" only a simple point on the contour.
        // p.I Field 4: The image
        FileMATLAB fileMATLAB = null;
        int r;
        double pointsx[];
        double pointsy[];
        int i;
        int j;
        int k;
        double pointst[];
        float px[];
        float py[];
        int numLoc;
        int iArr[];
        double linex[];
        double liney[];
        int numPoints;
        double dx[];
        double dy[];
        double dist[];
        double dist2[];
        double linex2[];
        double liney2[];
        double frac;
        double totalx[] = null;
        double totaly[] = null;
        double tempx[];
        double tempy[];
        
        // Function written by D. Kroon University of Twente (February 2010)
        try {
            fileMATLAB = new FileMATLAB(fileName, fileDir);
        }
        catch(IOException e) {
            MipavUtil.displayError("IOException " + e + " on fileMaTLAB = new FileMATLAB(fileName, fileDir)");
            fatalError = true;
            return;
        }
        
        try {
            fileMATLAB.readImage(false, true);
        }
        catch(IOException e) {
            MipavUtil.displayError("IOException " + e + " on fileMATLAB.readImage(false, true)");
            fatalError = true;
            return;
        }
        
        // Interpolate to get more points
        // For now use linear interpolation rather than the MATLAB lowpass FIR interpolating filter
        r = 5;
        pointsx = new double[r*p.x.length - r + 1];
        for (i = 0; i < p.x.length; i++) {
            pointsx[r*i] = p.x[i];
            if (i > 0) {
                for (j = 1; j < r; j++) {
                    pointsx[r*(i-1) + j] = p.x[i-1] + (j*(p.x[i] - p.x[i-1]))/r;
                }
            }
        }
        pointsy = new double[r*p.y.length - r + 1];
        for (i = 0; i < p.y.length; i++) {
            pointsy[r*i] = p.y[i];
            if (i > 0) {
                for (j = 1; j < r; j++) {
                    pointsy[r*(i-1) + j] = p.y[i-1] + (j*(p.y[i] - p.y[i-1]))/r;
                }
            }
        }
        
        // Mark landmark points with 1, other poinst zero
        pointst = new double[pointsx.length];
        for (i = 0; i < p.t.length; i++) {
            if (p.t[i] == 0) {
                pointst[i*r] = 1;
            }
        }
        
        if (verbose) {
            new ViewJFrameImage(p.I);
            // Add subsequent plots to the image
            // IMSHOW shows the first dimension of data along the y axis, so pointsy is plotted along the y axis
            // and pointsx is plotted along the x axis
            // 'b' means the color blue is used
            px = new float[pointsx.length];
            for (i = 0; i < pointsx.length; i++) {
                px[i] = (float)pointsx[i];
            }
            py = new float[pointsy.length];
            for (i = 0; i < pointsy.length; i++) {
                py[i] = (float)pointsy[i];
            }
            new ViewJFrameGraph(px, py, "Contour point locations", "pointsx", "pointsy", Color.BLUE);
        } // if (verbose)
        
        // Find the landmark point locations
        numLoc = 0;
        for (j = 0; j < pointst.length; j++) {
            if (pointst[j] != 0) {
                numLoc++;
            }
        }
        iArr = new int[numLoc];
        i = 0;
        for (j = 0; j < pointst.length; j++) {
            if (pointst[j] != 0) {
                iArr[i++] = j;    
            }
        }
        
        // Loop to make points evenly spaced on line pieces between landmark points
        for (j = 0; j < numLoc-1; j++) {
            // One line piece
            numPoints = iArr[j+1] - iArr[j] + 1;
            linex = new double[numPoints];
            liney = new double[numPoints];
            for (i = 0; i < numPoints; i++) {
                linex[i] = pointsx[iArr[j] + i];
                liney[i] = pointsy[iArr[j] + i];
            } // for (i = 0; i < numPoints; i++)
            // Length on line through the points
            dx = new double[numPoints];
            dy = new double[numPoints];
            for (i = 1; i < numPoints; i++) {
                dx[i] = linex[i] - linex[i-1];
                dy[i] = liney[i] - liney[i-1];
            } // for (i = 1; i < numPoints; i++)
            dist = new double[numPoints];
            dist[0] = 0.0;
            for (i = 1; i < numPoints; i++) {
                dist[i] = dist[i-1] + Math.sqrt(dx[i]*dx[i] + dy[i]*dy[i]);
            }
            // Interpolate new points evenly spaced on the line piece
            dist2 = new double[nBetween];
            for (i = 0; i < nBetween; i++) {
                dist2[i] = i * dist[numPoints-1]/(nBetween-1.0);
            }
            linex2 = new double[nBetween];
            liney2 = new double[nBetween];
            for (i = 0; i < nBetween; i++) {
                if (dist2[i] == dist[0]) {
                    linex2[i] = linex[0];
                    liney2[i] = liney[0];
                }
                else if (dist2[i] == dist[numPoints-1]) {
                    linex2[i] = linex[numPoints-1]; 
                    liney2[i] = liney[numPoints-1];
                }
                else {
                    k = 0;
                    while ((dist[k] <= dist2[i]) && (k < numPoints-1)) {
                        k++;
                    }
                    frac = (dist2[i] - dist[k-1])/(dist[k] - dist[k-1]);
                    linex2[i] = linex[k-1] + frac * (linex[k] - linex[k-1]);
                    liney2[i] = liney[k-1] + frac * (liney[k] - liney[k-1]);
                }
            } // for (i = 0; i < nBetween; i++)
            // Display the line piece
            if (verbose) {
                // No obvious way to plot on same ViewJFrameGraph    
            }
            // Remove point because it is also in the next line piece
            if (j < numLoc - 2) {
                linex = new double[nBetween-1];
                liney = new double[nBetween-1];
                for (i = 0; i < nBetween-1; i++) {
                    linex[i] = linex2[i];
                    liney[i] = liney2[i];
                }
            }
            else {
                linex = new double[nBetween];
                liney = new double[nBetween];
                for (i = 0; i < nBetween; i++) {
                    linex[i] = linex2[i];
                    liney[i] = liney2[i];
                }
            }
            // Add the evenly spaced line piece to the total line
            if (totalx == null) {
                totalx = new double[linex.length];
                for (i = 0; i < linex.length; i++) {
                    totalx[i] = linex[i];
                }
                totaly = new double[liney.length];
                for (i = 0; i < liney.length; i++) {
                    totaly[i] = liney[i];
                }
            }
            else {
                tempx = new double[totalx.length];
                for (i = 0; i < totalx.length; i++) {
                    tempx[i] = totalx[i];
                }
                tempy = new double[totaly.length];
                for (i = 0; i < totaly.length; i++) {
                    tempy[i] = totaly[i];
                }
                totalx = new double[tempx.length + linex.length];
                for (i = 0; i < tempx.length; i++) {
                    totalx[i] = tempx[i];
                }
                for (i = 0; i < linex.length; i++) {
                    totalx[tempx.length + i] = linex[i];
                }
                totaly = new double[tempy.length + liney.length];
                for (i = 0; i < tempy.length; i++) {
                    totaly[i] = tempy[i];
                }
                for (i = 0; i < liney.length; i++) {
                    totaly[tempy.length + i] = liney[i];
                }
            }
        } // for (j = 0; j < numLoc-1; j++)
        // Also store the image
        Vertices = new double[totalx.length][2];
        for (i = 0; i < totalx.length; i++) {
            Vertices[i][0] = totalx[i];
            Vertices[i][1] = totaly[i];
        }
        Lines = new int[totalx.length][2];
        for (i = 0; i < totalx.length; i++) {
            Lines[i][0] = i;
        }
        for (i = 1; i < totalx.length; i++) {
            Lines[i-1][1] = i;
        }
        Lines[totalx.length-1][1] = 0;
    }
    
    

    public class FileMATLAB extends FileBase {
        // .mat file data types
        /** 8 bit, signed */
        private static final int miINT8 = 1;
        /** 8 bit, unsigned */
        private static final int miUINT8 = 2;
        /** 16-bit, signed */
        private static final int miINT16 = 3;
        /** 16-bit, unsigned */
        private static final int miUINT16 = 4;
        /** 32-bit signed */
        private static final int miINT32 = 5;
        /** 32-bit, unsigned */
        private static final int miUINT32 = 6;
        /** IEEE 754 single format */
        private static final int miSINGLE = 7;
        /** IEEE 754 double format */
        private static final int miDOUBLE = 9;
        /** 64-bit, signed */
        private static final int miINT64 = 12;
        /** 64-bit, unsigned */
        private static final int miUINT64 = 13;
        /** MATLAB ARRAY */
        private static final int miMATRIX = 14;
        /** Compressed Data */
        private static final int miCOMPRESSED = 15;
        /** Unicode UTF-8 Encoded Character Data */
        private static final int miUTF8 = 16;
        /** Unicode UTF-16 Encoded Character Data */
        private static final int miUTF16 = 17;
        /** Unicode UTF-32 Encoded Character Data */
        private static final int miUTF32 = 18;
        
        // MATLAB Array Types (Classes)
        /** Cell array */
        private static final byte mxCELL_CLASS = 1;
        /** Structure */
        private static final byte mxSTRUCT_CLASS = 2;
        /** Object */
        private static final byte mxOBJECT_CLASS = 3;
        /** Character array */
        private static final byte mxCHAR_CLASS = 4;
        /** Sparse array */
        private static final byte mxSPARSE_CLASS = 5;
        /** Double precision array */
        private static final byte mxDOUBLE_CLASS = 6;
        /** Single precision array */
        private static final byte mxSINGLE_CLASS = 7;
        /** 8-bit, signed integer */
        private static final byte mxINT8_CLASS = 8;
        /** 8-bit, unsigned integer */
        private static final byte mxUINT8_CLASS = 9;
        /** 16-bit, signed integer */
        private static final byte mxINT16_CLASS = 10;
        /** 16-bit, unsigned integer */
        private static final byte mxUINT16_CLASS = 11;
        /** 32-bit, signed integer */
        private static final byte mxINT32_CLASS = 12;
        /** 32-bit, unsigned integer */
        private static final byte mxUINT32_CLASS = 13;
        /** 64-bit, signed integer */
        private static final byte mxINT64_CLASS = 14;
        /** 64-bit, unsigned integer */
        private static final byte mxUINT64_CLASS = 15;
        
        

        //~ Instance fields ------------------------------------------------------------------------------------------------

        /** DOCUMENT ME! */
        private File file;

        /** DOCUMENT ME! */
        private String fileDir;


        /** DOCUMENT ME! */
        private FileInfoMATLAB fileInfo;
        
        private FileInfoMATLAB fileInfo2;
        
        private FileInfoMATLAB maskFileInfo;
        
        private FileInfoMATLAB maskFileInfo2;

        /** DOCUMENT ME! */
        private String fileName;

        /** DOCUMENT ME! */
        private ModelImage image;

        /** DOCUMENT ME! */
        private int[] imageExtents = null;

        /** DOCUMENT ME! */
        private float[] imgBuffer = null;

        /** DOCUMENT ME! */
        private float[] imgResols = new float[5];

        /** DOCUMENT ME! */
        private ModelLUT LUT = null;
        
        private Inflater zlibDecompresser = null;
        
        private ModelImage image2 = null;
        
        private ViewJFrameImage vFrame2 = null;
        
        private ModelImage maskImage = null;
        
        private ViewJFrameImage vmFrame = null;
        
        private ModelImage maskImage2 = null;
        
        private ViewJFrameImage vmFrame2 = null;

        //~ Constructors ---------------------------------------------------------------------------------------------------

        /**
         * LIFF reader/writer constructor.
         *
         * @param      fileName  file name
         * @param      fileDir   file directory
         *
         * @exception  IOException  if there is an error making the file
         */
        public FileMATLAB(String fileName, String fileDir) throws IOException {

            this.fileName = fileName;
            this.fileDir = fileDir;
        }

        //~ Methods --------------------------------------------------------------------------------------------------------

        /**
         * Prepares this class for cleanup. Calls the <code>finalize</code> method for existing elements, closes any open
         * files and sets other elements to <code>null</code>.
         */
        public void finalize() {
            fileName = null;
            fileDir = null;
            fileInfo = null;
            file = null;
            image = null;
            imageExtents = null;
            imgBuffer = null;
            imgResols = null;
            LUT = null;
            
            try {
                super.finalize();
            } catch (Throwable er) { }
        }
        
        /**
         * Accessor that returns the file info.
         *
         * @return  FileInfoBase containing the file info
         */
        public FileInfoBase getFileInfo() {
            return fileInfo;
        }
        
        


        /**
         * Accessor that returns the image buffer.
         *
         * @return  buffer of image.
         */
        public float[] getImageBuffer() {
            return imgBuffer;
        }

        /**
         * Rreturns LUT if defined.
         *
         * @return  the LUT if defined else it is null
         */
        public ModelLUT getModelLUT() {
            return LUT;
        }
        
        /**
         * Reads the  MATLAB header.
         *
         * @param      multiFile  <code>true</code> if a set of files each containing a separate 2D image is present <code>
         *                        false</code> if one file with either a 2D image or a stack of 2D images
         * @param      one        <code>true</code> if only want to read in one image of the 3D set
         *
         * @return     returns the image
         *
         * @exception  IOException  if there is an error reading the file
         */
        public ModelImage readImage(boolean multiFile, boolean one) throws IOException {
            long fileLength;
            boolean endianess;
            int i, j;
            int imageSlices = 1;
            int imageSlices2 = 1;
            byte firstEndianByte;
            byte secondEndianByte;
            String headerTextField;
            byte firstByte;
            byte secondByte;
            byte thirdByte;
            byte fourthByte;
            boolean level5Format;
            long subsystemSpecificDataOffset;
            int version;
            long nextElementAddress;
            int dataType;
            int elementBytes;
            int padBytes;
            byte buffer[] = null;
            String str;
            boolean isCompressed = false;
            int s;
            File ufile = null;
            byte[] decomp = null;
            int resultLength = 0;
            int arrayFlagsDataType;
            int arrayFlagsBytes;
            int arrayFlags;
            boolean complexFlag = false;
            @SuppressWarnings("unused")
            boolean globalFlag = false;
            boolean logicalFlag = false;
            int arrayClass;
            int dimensionsArrayDataType;
            int dimensionsArrayBytes;
            int nDim;
            int arrayNameDataType;
            int arrayNameBytes;
            String arrayName;
            int realDataType;
            int realDataBytes;
            int imaginaryDataType;
            int imaginaryDataBytes;
            short shortBuffer[] = null;
            int intBuffer[] = null;
            long longBuffer[] = null;
            float floatBuffer[] = null;
            float realBuffer[] = null;
            float imaginaryBuffer[] = null;
            double doubleBuffer[] = null;
            double realDBuffer[] = null;
            double imaginaryDBuffer[] = null;
            byte tBuffer[] = null;
            int imageLength = 1;
            int x;
            int y;
            String uncompressedName = null;
            int imagesFound = 0;
            int structureDimensions[];
            int maximumFieldNameLengthBytes;
            int maximumFieldNameLengthDataType;
            int maximumFieldNameLength;
            int fieldNamesDataType;
            int fieldNamesBytes;
            int fieldNumber = 1;
            String fieldNames[] = null;
            int bytesRead;
            int field;
            int numericArrayDataType;
            int numericArrayBytes;
            int numericArrayFlagsDataType;
            int numericArrayFlagsBytes;
            int numericArrayFlags;
            int numericArrayClass;
            int numericArrayNameDataType;
            int numericArrayNameBytes;
            String numericArrayName;
            int logicalFields = 0;
            boolean isVOI = false;
            boolean isVOI2 = false;
            int nonLogicalField = 0;
            int adjustedFieldDim;
            int newExtents[] = null;
            int totalNumber = 1;
            boolean logMagDisplay = Preferences.is(Preferences.PREF_LOGMAG_DISPLAY);
            int maskExtents[] = null;
            boolean haveSmallRealData;
            boolean haveSmallImaginaryData;
            String voiFieldName = null;
            String voi2FieldName = null;
            int sliceSize;
            int index;
            int shortNumber;
            int intNumber;
            int longNumber;
            int floatNumber;
            int doubleNumber;
            int b1;
            int b2;
            int b3;
            int b4;
            long b1L;
            long b2L;
            long b3L;
            long b4L;
            long b5L;
            long b6L;
            long b7L;
            long b8L;
            int tmpInt;
            long tmpLong;
            boolean isColor = false;
            int numberSlices;
            int st;
            int elementNumber = 0;
            int totalNumber2;
            int shortMin;
            int shortMax;
            int shortRange;
            int maxColorIndex;
            double scale;
            double scaledIndex;
            int lowIndex;
            int highIndex;
            double lowFraction;
            double highFraction;
            byte buf[] = null;
            int bufSize;
            boolean memoryError;

            try {
                p = new P();
                imgResols[0] = imgResols[1] = imgResols[2] = imgResols[3] = imgResols[4] = (float) 1.0;
                file = new File(fileDir + fileName);
                raFile = new RandomAccessFile(file, "r");
                st = fileName.lastIndexOf(".");
                
                fileLength = raFile.length();
                Preferences.debug("fileLength = " + fileLength + "\n", Preferences.DEBUG_FILEIO);
                raFile.seek(126L);
                firstEndianByte = raFile.readByte();
                secondEndianByte = raFile.readByte();
                if ((firstEndianByte == 77) && (secondEndianByte == 73)) {
                    // M followed by I
                    endianess = FileBase.BIG_ENDIAN;
                    Preferences.debug("The MATLAB file is big endian\n", Preferences.DEBUG_FILEIO);
                }
                else if ((firstEndianByte == 73) && (secondEndianByte == 77)) {
                    // I followed by M
                    endianess = FileBase.LITTLE_ENDIAN;
                    Preferences.debug("The MATLAB file is little endian\n", Preferences.DEBUG_FILEIO);
                }
                else {
                    raFile.close();
                    throw new IOException("MATLAB Read Header: Endian Indicator at bytes 126 and 127 are illegal " + firstEndianByte + 
                                             " followed by " + secondEndianByte);
                }
                
                fileInfo = new FileInfoMATLAB(fileName, fileDir, FileUtility.MATLAB); // dummy fileInfo
                fileInfo.setEndianess(endianess);
                
                raFile.seek(0L);
                firstByte = raFile.readByte();
                secondByte = raFile.readByte();
                thirdByte = raFile.readByte();
                fourthByte = raFile.readByte();
                if ((firstByte == 0) || (secondByte == 0) || (thirdByte == 0) || (fourthByte == 0)) {
                     // MATLAB uses level 4 format
                     level5Format = false;  
                     Preferences.debug("The MATLAB file uses level 4 format\n", Preferences.DEBUG_FILEIO);
                }
                else {
                    // MATLAB uses level 5 format
                    level5Format = true;
                    Preferences.debug("The MATLAB file uses level 5 format\n", Preferences.DEBUG_FILEIO);
                }
                
                fileInfo.setLevel5Format(level5Format);
                
                raFile.seek(0L);
                
                headerTextField = getString(116);
                Preferences.debug("Header text field = " + headerTextField.trim() + "\n", Preferences.DEBUG_FILEIO);
                fileInfo.setHeaderTextField(headerTextField);
                
                // Location 116
                subsystemSpecificDataOffset = getLong(endianess);
                // All zeros or all spaces in this field indicate that there is no 
                // subsystem-specific data stored in this file
                if ((subsystemSpecificDataOffset == 0L) || (subsystemSpecificDataOffset == 0x2020202020202020L)) {
                    Preferences.debug("No subsystem specific data stored in file\n", Preferences.DEBUG_FILEIO);
                }
                else {
                    Preferences.debug("Subystem specific data stored at location " + subsystemSpecificDataOffset + "\n", 
                            Preferences.DEBUG_FILEIO);
                    fileInfo.setSubsystemSpecificDataOffset(subsystemSpecificDataOffset);
                }
                
                // Location 124
                version = getUnsignedShort(endianess);
                if (version == 256) {
                    Preferences.debug("The version number is the expected 256\n", Preferences.DEBUG_FILEIO);    
                }
                else {
                    Preferences.debug("The version number = " + version + " instead of the expected 256\n", 
                            Preferences.DEBUG_FILEIO);
                }
                fileInfo.setVersion(version);
                
                // Go to first data element location
                nextElementAddress = 128L;
                while (nextElementAddress < fileLength) {
                    elementNumber++;
                    isCompressed = false;
                    raFile.seek(nextElementAddress);
                    dataType = getInt(endianess);
                    if ((dataType & 0xffff0000) != 0) {
                        // Small Data Element Format
                        elementBytes = (dataType & 0xffff0000) >>> 16;
                        dataType = dataType & 0xffff;
                        nextElementAddress = nextElementAddress + 8;
                    }
                    else {
                        elementBytes = getInt(endianess);
                        // Must pad to make sure the tag of the next data element
                        // falls on a 64-bit boundary.
                        padBytes = 0;
                        if ((elementBytes % 8) != 0) {
                            padBytes = 8 - (elementBytes % 8);
                        }
                        if (dataType == miCOMPRESSED) {
                            nextElementAddress = nextElementAddress + elementBytes + 8;
                        }
                        else {
                            nextElementAddress = nextElementAddress + elementBytes + padBytes + 8;
                        }
                    }
                    Preferences.debug("nextElementAddress = " + nextElementAddress + "\n", Preferences.DEBUG_FILEIO);
                    if (dataType == miCOMPRESSED) {
                        fireProgressStateChanged("Decompressing element number " + String.valueOf(elementNumber));
                        if (elementNumber == 1) {
                            fireProgressStateChanged(25);
                        }
                        else if (elementNumber == 2) {
                            fireProgressStateChanged(60);
                        }
                        else {
                            fireProgressStateChanged(85);
                        }
                        isCompressed = true;
                        Preferences.debug("Data type = miCOMPRESSED\n", Preferences.DEBUG_FILEIO);
                        Preferences.debug("Bytes in data element = " + elementBytes + "\n", Preferences.DEBUG_FILEIO);
                        buffer = new byte[elementBytes];
                        raFile.read(buffer);
                        zlibDecompresser = new Inflater();
                        zlibDecompresser.setInput(buffer);
                        
                        // Create an expandable byte array to hand the decompressed data
                        ByteArrayOutputStream bos = new ByteArrayOutputStream(buffer.length);
                        
                        // Decompress the data
                        // Let buf be the smallest power of 2 which is at least 65536 and twice the uncompressed
                        // size so as to balance the need for speed against excessive memory use
                        // The maximum integer value is 2**31 -1, so limit size to 2**30.
                        // Cast elementBytes to long in while loop in case elementBytes >= 2**30.
                        bufSize = 65536;
                        while ((bufSize < 2*(long)elementBytes) && (bufSize < (int)Math.pow(2,30))){
                            bufSize *= 2;
                        }
                        memoryError = true;
                        while (memoryError) {
                            try {
                                 buf = new byte[bufSize];
                                 memoryError = false;
                            }
                            catch (OutOfMemoryError e) {
                                bufSize = bufSize/2;
                                memoryError = true;
                            }
                        }
                        try {
                            while (true) {
                                int count = zlibDecompresser.inflate(buf);
                                if (count > 0) {
                                    bos.write(buf, 0 , count);
                                }
                                else if (count == 0 && zlibDecompresser.finished()) {
                                    break;
                                } else  {
                                    throw new RuntimeException("bad zip data, size:" + buffer.length);
                                } 
                            }
                        } catch (Throwable t) {
                            throw new RuntimeException(t);
                        } finally {
                            zlibDecompresser.end();
                        }
                        // Get the decompressed data
                        decomp = bos.toByteArray();
                        resultLength = decomp.length;
                        uncompressedName = fileDir + fileName.substring(0, st) + "uncompressed.mat";
                        ufile = new File(uncompressedName);
                        raFile = new RandomAccessFile(ufile, "rw");
                        raFile.setLength(0);
                        raFile.write(decomp, 0, resultLength);
                        decomp = null;
                        raFile.seek(0L);
                        dataType = getInt(endianess);
                        if ((dataType & 0xffff0000) != 0) {
                            // Small Data Element Format
                            elementBytes = (dataType & 0xffff0000) >>> 16;
                            dataType = dataType & 0xffff;
                        }
                        else {
                            elementBytes = getInt(endianess);
                        }
                    } // if (dataTypeCOMPRESSED)
                    fireProgressStateChanged("Reading element number " + String.valueOf(elementNumber));
                    if (elementNumber == 1) {
                        fireProgressStateChanged(50);
                    }
                    else if (elementNumber == 2) {
                        fireProgressStateChanged(75);
                    }
                    else {
                        fireProgressStateChanged(85);
                    }
                    switch(dataType) {
                    case miINT8:
                        Preferences.debug("Data type = miINT8\n", Preferences.DEBUG_FILEIO);
                        Preferences.debug("Bytes in data element = " + elementBytes + "\n", Preferences.DEBUG_FILEIO);
                        break;
                    case miUINT8:
                        Preferences.debug("Data type = miUINT8\n", Preferences.DEBUG_FILEIO);
                        Preferences.debug("Bytes in data element = " + elementBytes + "\n", Preferences.DEBUG_FILEIO);
                        break;
                    case miINT16:
                        Preferences.debug("Data type = miINT16\n", Preferences.DEBUG_FILEIO);
                        Preferences.debug("Bytes in data element = " + elementBytes + "\n", Preferences.DEBUG_FILEIO);
                        break;
                    case miUINT16:
                        Preferences.debug("Data type = miUINT16\n", Preferences.DEBUG_FILEIO);
                        Preferences.debug("Bytes in data element = " + elementBytes + "\n", Preferences.DEBUG_FILEIO);
                        break;
                    case miINT32:
                        Preferences.debug("Data type = miINT32\n", Preferences.DEBUG_FILEIO);
                        Preferences.debug("Bytes in data element = " + elementBytes + "\n", Preferences.DEBUG_FILEIO);
                        break;
                    case miUINT32:
                        Preferences.debug("Data type = miUINT32\n", Preferences.DEBUG_FILEIO);
                        Preferences.debug("Bytes in data element = " + elementBytes + "\n", Preferences.DEBUG_FILEIO);
                        break;
                    case miSINGLE:
                        Preferences.debug("Data type = miSINGLE\n", Preferences.DEBUG_FILEIO);
                        Preferences.debug("Bytes in data element = " + elementBytes + "\n", Preferences.DEBUG_FILEIO);
                        break;
                    case miDOUBLE:
                        Preferences.debug("Data type = miDOUBLE\n", Preferences.DEBUG_FILEIO);
                        Preferences.debug("Bytes in data element = " + elementBytes + "\n", Preferences.DEBUG_FILEIO);
                        break;
                    case miINT64:
                        Preferences.debug("Data type = miINT64\n", Preferences.DEBUG_FILEIO);
                        Preferences.debug("Bytes in data element = " + elementBytes + "\n", Preferences.DEBUG_FILEIO);
                        break;
                    case miUINT64:
                        Preferences.debug("Data type = miUINT64\n", Preferences.DEBUG_FILEIO);
                        Preferences.debug("Bytes in data element = " + elementBytes + "\n", Preferences.DEBUG_FILEIO);
                        break;
                    case miMATRIX:
                        Preferences.debug("Data type = miMATRIX\n", Preferences.DEBUG_FILEIO);
                        Preferences.debug("Bytes in data element = " + elementBytes + "\n", Preferences.DEBUG_FILEIO);
                        imageExtents = null;
                        imagesFound++;
                        logicalFields = 0;
                        arrayFlagsDataType = getInt(endianess);
                        if (arrayFlagsDataType == miUINT32) {
                            Preferences.debug("Array flags data type is the expected miUINT32\n", Preferences.DEBUG_FILEIO);
                        }
                        else {
                            Preferences.debug("Array flags data type is an unexpected " + arrayFlagsDataType + "\n", 
                                    Preferences.DEBUG_FILEIO);
                        }
                        arrayFlagsBytes = getInt(endianess);
                        if (arrayFlagsBytes == 8) {
                            Preferences.debug("Array flags byte length = 8 as expected\n", Preferences.DEBUG_FILEIO);
                        }
                        else {
                            Preferences.debug("Array flags byte length is an unexpected " + arrayFlagsBytes + "\n", 
                                    Preferences.DEBUG_FILEIO);
                        }
                        arrayFlags = getInt(endianess);
                        arrayClass = arrayFlags & 0x000000ff;
                        switch(arrayClass) {
                        case mxCELL_CLASS:
                            Preferences.debug("Array type is cell array\n", Preferences.DEBUG_FILEIO);
                            break;
                        case mxSTRUCT_CLASS:
                            Preferences.debug("Array type is structure\n", Preferences.DEBUG_FILEIO);
                            break;
                        case mxOBJECT_CLASS:
                            Preferences.debug("Array type is object\n", Preferences.DEBUG_FILEIO);
                            break;
                        case mxCHAR_CLASS:
                            Preferences.debug("Array type is character\n", Preferences.DEBUG_FILEIO);
                            break;
                        case mxSPARSE_CLASS:
                            Preferences.debug("Array type is sparse\n", Preferences.DEBUG_FILEIO);
                            break;
                        case mxDOUBLE_CLASS:
                            Preferences.debug("Array type is 8 byte double\n", Preferences.DEBUG_FILEIO);
                            break;
                        case mxSINGLE_CLASS:
                            Preferences.debug("Array type is 4 byte float\n", Preferences.DEBUG_FILEIO);
                            break;
                        case mxINT8_CLASS:
                            Preferences.debug("Array type is signed byte\n", Preferences.DEBUG_FILEIO);
                            break;
                        case mxUINT8_CLASS:
                            Preferences.debug("Array type is unsigned byte\n", Preferences.DEBUG_FILEIO);
                            break;
                        case mxINT16_CLASS:
                            Preferences.debug("Array type is signed short\n", Preferences.DEBUG_FILEIO);
                            break;
                        case mxUINT16_CLASS:
                            Preferences.debug("Array type is unsigned short\n", Preferences.DEBUG_FILEIO);
                            break;
                        case mxINT32_CLASS:
                            Preferences.debug("Array type is signed integer\n", Preferences.DEBUG_FILEIO);
                            break;
                        case mxUINT32_CLASS:
                            Preferences.debug("Array type is unsigned integer\n", Preferences.DEBUG_FILEIO);
                            break;
                        case mxINT64_CLASS:
                            Preferences.debug("Array type is signed long\n", Preferences.DEBUG_FILEIO);
                            break;
                        case mxUINT64_CLASS:
                            Preferences.debug("Array type is unsigned long\n", Preferences.DEBUG_FILEIO);
                            break;
                        default:
                            Preferences.debug("Array type is an illegal = " + arrayClass + "\n", Preferences.DEBUG_FILEIO);
                        }
                        if (arrayClass == mxCHAR_CLASS) {
                            imagesFound--;
                            if (isCompressed) {
                                raFile.close();
                                try {
                                    ufile.delete();
                                } catch (final SecurityException sc) {
                                    MipavUtil.displayError("Security error occurs while trying to delete " + uncompressedName);
                                }
                                raFile = new RandomAccessFile(file, "r");
                            }
                            continue;
                        }
                        if (imagesFound == 2) {
                            fileInfo2 = new FileInfoMATLAB(fileName + "_2", fileDir, FileUtility.MATLAB); // dummy fileInfo
                            fileInfo2.setEndianess(endianess);
                            fileInfo2.setLevel5Format(level5Format);
                            fileInfo2.setHeaderTextField(headerTextField);
                            fileInfo2.setVersion(version);
                        }
                        if ((arrayFlags & 0x00000800) != 0) {
                            complexFlag = true;
                            Preferences.debug("Complex flag is set\n", Preferences.DEBUG_FILEIO);
                        }
                        else {
                            complexFlag = false;
                            Preferences.debug("Complex flag is not set\n", Preferences.DEBUG_FILEIO);
                        }
                        if ((arrayFlags & 0x00000400) != 0) {
                            globalFlag = true;
                            Preferences.debug("Global flag is set\n", Preferences.DEBUG_FILEIO);
                        }
                        else {
                            globalFlag = false;
                            Preferences.debug("Global flag is not set\n", Preferences.DEBUG_FILEIO);
                        }
                        if ((arrayFlags & 0x00000200) != 0) {
                            logicalFlag = true;
                            Preferences.debug("Logical flag is set\n", Preferences.DEBUG_FILEIO);
                        }
                        else {
                            logicalFlag = false;
                            Preferences.debug("Logical flag is not set\n", Preferences.DEBUG_FILEIO);
                        }
                        // 4 undefined bytes
                        getInt(endianess);
                        dimensionsArrayDataType = getInt(endianess);
                        if (dimensionsArrayDataType == miINT32) {
                            Preferences.debug("Dimensions array data type is the expected miINT32\n", Preferences.DEBUG_FILEIO);
                        }
                        else {
                            Preferences.debug("Dimensions array data type is an unexpected " + dimensionsArrayDataType + "\n", 
                                    Preferences.DEBUG_FILEIO);
                        }
                        dimensionsArrayBytes = getInt(endianess);
                        Preferences.debug("dimensionsArrayBytes = " + dimensionsArrayBytes + "\n", Preferences.DEBUG_FILEIO);
                        if ((dimensionsArrayBytes % 4) == 0) {
                            Preferences.debug("dimensionsArrayBytes is a multiple of 4 as expected\n", Preferences.DEBUG_FILEIO);
                        }
                        else {
                            Preferences.debug("dimensionArrayBytes is unexpectedly not a multiple of 4\n", 
                                    Preferences.DEBUG_FILEIO);
                        }
                        nDim = dimensionsArrayBytes/4;
                        Preferences.debug("Number of dimensions = " + nDim + "\n", Preferences.DEBUG_FILEIO);
                        if (nDim < 2) {
                            Preferences.debug("Error! All numeric arrays should have at least 2 dimensions\n",
                                    Preferences.DEBUG_FILEIO);
                        }
                        if (arrayClass == mxSTRUCT_CLASS) {
                            structureDimensions = new int[nDim];
                            for (i = 0; i < nDim; i++) {
                                // Ignore structure dimensions
                                structureDimensions[i] = getInt(endianess);
                                Preferences.debug("Ignored structureDimensions[" + i + " ] = " + structureDimensions[i] + "\n", 
                                        Preferences.DEBUG_FILEIO);
                            }
                        }
                        else { // arrayClass != mxSTRUCT_CLASS
                            imageExtents = new int[nDim];
                            imageLength = 1;
                            if (imagesFound == 1) {
                                imageSlices = 1;
                            }
                            else {
                                imageSlices2 = 1;
                            }
                            for (i = 0; i < nDim; i++) {
                                if (i == 0) {
                                    imageExtents[1] = getInt(endianess);
                                    Preferences.debug("imageExtents[1] = " + imageExtents[1] + "\n", Preferences.DEBUG_FILEIO);
                                }
                                else if (i == 1) {
                                    imageExtents[0] = getInt(endianess);
                                    Preferences.debug("imageExtents[0] = " + imageExtents[0] + "\n", Preferences.DEBUG_FILEIO);
                                }
                                else {
                                    imageExtents[i] = getInt(endianess);
                                    Preferences.debug("imageExtents["+ i + "] = " + imageExtents[i] + "\n", 
                                            Preferences.DEBUG_FILEIO);
                                }
                                imageLength = imageLength * imageExtents[i];
                                if (i > 1) {
                                    if (imagesFound == 1) {
                                        imageSlices = imageSlices * imageExtents[i];
                                    }
                                    else {
                                        imageSlices2 = imageSlices2 * imageExtents[i];
                                    }
                                }
                            }
                            if ((imageExtents[0] == 1) || (imageExtents[1] == 1)) {
                                imagesFound--;
                                if (isCompressed) {
                                    raFile.close();
                                    try {
                                        ufile.delete();
                                    } catch (final SecurityException sc) {
                                        MipavUtil.displayError("Security error occurs while trying to delete " + uncompressedName);
                                    }
                                    raFile = new RandomAccessFile(file, "r");
                                }
                                continue;   
                            }
                            if ((nDim == 4) && (imageExtents[2] == 1)) {
                                nDim = 3;
                                newExtents = new int[3];
                                newExtents[0] = imageExtents[0];
                                newExtents[1] = imageExtents[1];
                                newExtents[2] = imageExtents[3];
                                imageExtents = new int[3];
                                imageExtents[0] = newExtents[0];
                                imageExtents[1] = newExtents[1];
                                imageExtents[2] = newExtents[2];
                            }
                            if (imagesFound == 1) {
                                fileInfo.setExtents(imageExtents);
                            }
                            else {
                                fileInfo2.setExtents(imageExtents);
                            }
                        } // else arrayClass != mxSTRUCT_CLASS
                        if ((dimensionsArrayBytes % 8) != 0) {
                            // Skip over padding bytes
                            padBytes = 8 - (dimensionsArrayBytes % 8);
                            for (i = 0; i < padBytes; i++) {
                                raFile.readByte();
                            }
                        } // if ((dimensionsArrayBytes % 8) != 0)
                        arrayNameDataType = getInt(endianess);
                        if ((arrayNameDataType & 0xffff0000) != 0) {
                            // Small data element format    
                            arrayNameBytes = (arrayNameDataType & 0xffff0000) >>> 16;
                            arrayNameDataType = arrayNameDataType & 0xffff;
                            arrayName = getString(arrayNameBytes);
                            if (arrayNameBytes < 4) {
                                for (i = 0; i < 4 - arrayNameBytes; i++) {
                                    // Skip over padding bytes
                                    raFile.readByte();
                                }
                            }
                        }
                        else {
                            arrayNameBytes = getInt(endianess);
                            arrayName = getString(arrayNameBytes);
                            // Skip over padding bytes
                            if ((arrayNameBytes % 8) != 0) {
                                padBytes = 8 - (arrayNameBytes % 8);
                                for (i = 0; i < padBytes; i++) {
                                    raFile.readByte();
                                }
                            }
                        }
                        Preferences.debug("Array name = " + arrayName + "\n", Preferences.DEBUG_FILEIO);
                        if (imagesFound == 1) {
                            fileInfo.setArrayName(arrayName);
                        }
                        else {
                            fileInfo2.setArrayName(arrayName);
                        }
                        if (arrayClass == mxSTRUCT_CLASS) {
                          // The field name length subelement always uses the compressed data element format
                            maximumFieldNameLengthDataType = getInt(endianess);
                            maximumFieldNameLengthBytes = (maximumFieldNameLengthDataType & 0xffff0000) >>> 16;
                            maximumFieldNameLengthDataType = maximumFieldNameLengthDataType & 0xffff;
                            if (maximumFieldNameLengthDataType == miINT32) {
                                Preferences.debug("maximumFieldNameLengthDataType == miINT32 as expected\n", 
                                        Preferences.DEBUG_FILEIO);
                            }
                            else {
                                Preferences.debug("maximumFieldNameLengthDataType unexpectedly == " + 
                                        maximumFieldNameLengthDataType + "\n", Preferences.DEBUG_FILEIO);
                            }
                            if (maximumFieldNameLengthBytes == 4) {
                                Preferences.debug("maximumFieldNameLengthBytes == 4 as expected\n", Preferences.DEBUG_FILEIO);
                            }
                            else {
                                Preferences.debug("maximumFieldNameLengthBytes == " + maximumFieldNameLengthBytes +
                                                  " instead of the expected 4\n", Preferences.DEBUG_FILEIO);
                            }
                            maximumFieldNameLength = getInt(endianess);
                            Preferences.debug("maximumFieldNameLength including null terminator = " + 
                                    maximumFieldNameLength + "\n", Preferences.DEBUG_FILEIO);
                            if (maximumFieldNameLength > 32) {
                                Preferences.debug("maximumFieldNameLength should not be greater than 32\n", 
                                        Preferences.DEBUG_FILEIO);
                            }
                            fieldNamesDataType = getInt(endianess);
                            if (fieldNamesDataType == miINT8) {
                                Preferences.debug("fieldNamesDataType == miINT8 as expected\n", Preferences.DEBUG_FILEIO);
                            }
                            else {
                                Preferences.debug("fieldNamesDataType unexpectely == " + fieldNamesDataType + "\n", 
                                        Preferences.DEBUG_FILEIO);
                            }
                            fieldNamesBytes = getInt(endianess);
                            Preferences.debug("fieldNamesBytes = " + fieldNamesBytes + "\n", Preferences.DEBUG_FILEIO);
                            if ((fieldNamesBytes % maximumFieldNameLength) == 0) {
                                Preferences.debug("fieldNamesBytes % maximumFieldNameLength == 0 as expected\n", 
                                        Preferences.DEBUG_FILEIO);
                            }
                            else {
                                Preferences.debug("fieldNamesBytes % maximumFieldNameLength unexpectedly == " +
                                        (fieldNamesBytes % maximumFieldNameLength) + "\n", Preferences.DEBUG_FILEIO);
                            }
                            fieldNumber = fieldNamesBytes / maximumFieldNameLength;
                            Preferences.debug("Field number = " + fieldNumber + "\n", Preferences.DEBUG_FILEIO);
                            fieldNames = new String[fieldNumber];
                            for (i = 0; i < fieldNumber; i++) {
                                fieldNames[i] = readCString();
                                Preferences.debug("field name " + i + " = " + fieldNames[i] + "\n", Preferences.DEBUG_FILEIO);
                                bytesRead = fieldNames[i].length() + 1;
                                padBytes = maximumFieldNameLength - bytesRead;
                                for (j = 0; j < padBytes; j++) {
                                    raFile.readByte();
                                }
                            }
                            if (imagesFound == 1) {
                                fileInfo.setFieldNames(fieldNames);
                            }
                            else {
                                fileInfo2.setFieldNames(fieldNames);
                            }
                            if ((fieldNamesBytes % 8) != 0) {
                                padBytes = 8 - (fieldNamesBytes % 8);
                                for (i = 0; i < padBytes; i++) {
                                    raFile.readByte();
                                }
                            }
                        } // if (arrayClass == mxSTRUCT_CLASS)
                        for (field = 0; field < fieldNumber; field++) {
                        if (arrayClass == mxSTRUCT_CLASS) {
                            Preferences.debug("Reading numeric array number " + field + "\n", Preferences.DEBUG_FILEIO);
                            numericArrayDataType = getInt(endianess);
                            if (numericArrayDataType == miMATRIX) {
                                Preferences.debug("Numeric array data type == miMATRIX as expected\n");
                            }
                            else {
                                Preferences.debug("Numeric array data type unexpectedly == " + numericArrayDataType + "\n", 
                                        Preferences.DEBUG_FILEIO);
                            }
                            numericArrayBytes= getInt(endianess);
                            Preferences.debug("Numeric array bytes = " + numericArrayBytes + "\n", Preferences.DEBUG_FILEIO);
                            numericArrayFlagsDataType = getInt(endianess);
                            if (arrayFlagsDataType == miUINT32) {
                                Preferences.debug("Numeric array flags data type is the expected miUINT32\n",
                                        Preferences.DEBUG_FILEIO);
                            }
                            else {
                                Preferences.debug("Numeric array flags data type is an unexpected " + numericArrayFlagsDataType +
                                        "\n", Preferences.DEBUG_FILEIO);
                            }
                            numericArrayFlagsBytes = getInt(endianess);
                            if (numericArrayFlagsBytes == 8) {
                                Preferences.debug("Numeric array flags byte length = 8 as expected\n", Preferences.DEBUG_FILEIO);
                            }
                            else {
                                Preferences.debug("Numeric array flags byte length is an unexpected " + numericArrayFlagsBytes +
                                        "\n", Preferences.DEBUG_FILEIO);
                            }
                            numericArrayFlags = getInt(endianess);
                            numericArrayClass = numericArrayFlags & 0x000000ff;
                            switch(numericArrayClass) {
                            case mxCELL_CLASS:
                                Preferences.debug("Numeric array type is cell array\n", Preferences.DEBUG_FILEIO);
                                break;
                            case mxSTRUCT_CLASS:
                                Preferences.debug("Numeric array type is structure\n", Preferences.DEBUG_FILEIO);
                                break;
                            case mxOBJECT_CLASS:
                                Preferences.debug("Numeric array type is object\n", Preferences.DEBUG_FILEIO);
                                break;
                            case mxCHAR_CLASS:
                                Preferences.debug("Numeric array type is character\n", Preferences.DEBUG_FILEIO);
                                break;
                            case mxSPARSE_CLASS:
                                Preferences.debug("Numereic array type is sparse\n", Preferences.DEBUG_FILEIO);
                                break;
                            case mxDOUBLE_CLASS:
                                Preferences.debug("Numeric array type is 8 byte float\n", Preferences.DEBUG_FILEIO);
                                break;
                            case mxSINGLE_CLASS:
                                Preferences.debug("Numeric array type is 4 byte float\n", Preferences.DEBUG_FILEIO);
                                break;
                            case mxINT8_CLASS:
                                Preferences.debug("Numeric array type is signed byte\n", Preferences.DEBUG_FILEIO);
                                break;
                            case mxUINT8_CLASS:
                                Preferences.debug("Numeric array type is unsigned byte\n", Preferences.DEBUG_FILEIO);
                                break;
                            case mxINT16_CLASS:
                                Preferences.debug("Numeric array type is signed short\n", Preferences.DEBUG_FILEIO);
                                break;
                            case mxUINT16_CLASS:
                                Preferences.debug("Numeric array type is unsigned short\n", Preferences.DEBUG_FILEIO);
                                break;
                            case mxINT32_CLASS:
                                Preferences.debug("Numeric array type is signed integer\n", Preferences.DEBUG_FILEIO);
                                break;
                            case mxUINT32_CLASS:
                                Preferences.debug("Numeric array type is unsigned integer\n", Preferences.DEBUG_FILEIO);
                                break;
                            default:
                                Preferences.debug("Numeric array type is an illegal = " + numericArrayClass + "\n", 
                                        Preferences.DEBUG_FILEIO);
                            }
                            
                            if ((numericArrayFlags & 0x00000800) != 0) {
                                complexFlag = true;
                                Preferences.debug("Complex flag is set\n", Preferences.DEBUG_FILEIO);
                            }
                            else {
                                complexFlag = false;
                                Preferences.debug("Complex flag is not set\n", Preferences.DEBUG_FILEIO);
                            }
                            if ((numericArrayFlags & 0x00000400) != 0) {
                                globalFlag = true;
                                Preferences.debug("Global flag is set\n", Preferences.DEBUG_FILEIO);
                            }
                            else {
                                globalFlag = false;
                                Preferences.debug("Global flag is not set\n", Preferences.DEBUG_FILEIO);
                            }
                            if ((numericArrayFlags & 0x00000200) != 0) {
                                logicalFlag = true;
                                Preferences.debug("Logical flag is set\n", Preferences.DEBUG_FILEIO);
                                logicalFields++;
                                if (imagesFound == 1) {
                                    isVOI = true;
                                }
                                else {
                                    isVOI2 = true;
                                }
                            }
                            else {
                                logicalFlag = false;
                                Preferences.debug("Logical flag is not set\n", Preferences.DEBUG_FILEIO);
                                if (imagesFound == 1) {
                                    isVOI = false;
                                }
                                else {
                                    isVOI2 = false;
                                }
                            }
                            nonLogicalField = field - logicalFields;
                            if ((imagesFound == 1) && (fieldNumber == 2) && (fieldNames != null) && logicalFlag) {
                                if (field == 0) {
                                    voiFieldName = fieldNames[0];
                                }
                                else {
                                    voiFieldName = fieldNames[1];
                                }
                            }
                            if ((imagesFound == 2) && (fieldNumber == 2) && (fieldNames != null) && logicalFlag) {
                                if (field == 0) {
                                    voi2FieldName = fieldNames[0];
                                }
                                else {
                                    voi2FieldName = fieldNames[1];
                                }
                            }
                            
                            // 4 undefined bytes
                            getInt(endianess);
                            dimensionsArrayDataType = getInt(endianess);
                            if (dimensionsArrayDataType == miINT32) {
                                Preferences.debug("Dimensions array data type is the expected miINT32\n", Preferences.DEBUG_FILEIO);
                            }
                            else {
                                Preferences.debug("Dimensions array data type is an unexpected " + dimensionsArrayDataType + "\n", 
                                        Preferences.DEBUG_FILEIO);
                            }
                            dimensionsArrayBytes = getInt(endianess);
                            Preferences.debug("dimensionsArrayBytes = " + dimensionsArrayBytes + "\n", Preferences.DEBUG_FILEIO);
                            if ((dimensionsArrayBytes % 4) == 0) {
                                Preferences.debug("dimensionsArrayBytes is a multiple of 4 as expected\n", 
                                        Preferences.DEBUG_FILEIO);
                            }
                            else {
                                Preferences.debug("dimensionArrayBytes is unexpectedly not a multiple of 4\n", 
                                        Preferences.DEBUG_FILEIO);
                            }
                            nDim = dimensionsArrayBytes/4;
                            Preferences.debug("Number of dimensions = " + nDim + "\n", Preferences.DEBUG_FILEIO);
                            if (nDim < 2) {
                                Preferences.debug("Error! All numeric arrays should have at least 2 dimensions\n", 
                                        Preferences.DEBUG_FILEIO);
                            }
                            
                            if (logicalFlag) {
                                // MATLAB function used to create VOIs (roipoly) always returns a 2D mask
                                maskExtents = new int[nDim];
                                for (i = 0; i < nDim; i++) {
                                    maskExtents[i] = getInt(endianess);
                                }
                                if (imageExtents != null) {
                                    for (i = 0; i < 2; i++) {
                                        if (imageExtents[i] != maskExtents[i]) {
                                            if (imagesFound == 1) {
                                                isVOI = false;
                                            }
                                            else {
                                                isVOI2 = false;
                                            }
                                        }
                                    }
                                }
                                if (imagesFound == 1) {
                                    maskFileInfo = new FileInfoMATLAB(fileName + "_mask", fileDir, FileUtility.MATLAB); // dummy fileInfo
                                    maskFileInfo.setEndianess(endianess);
                                    maskFileInfo.setLevel5Format(level5Format);
                                    maskFileInfo.setHeaderTextField(headerTextField);
                                    maskFileInfo.setVersion(version);
                                    maskFileInfo.setExtents(maskExtents);
                                    maskFileInfo.setArrayName(arrayName);
                                }
                                else {
                                    maskFileInfo2 = new FileInfoMATLAB(fileName + "_2_mask", fileDir, FileUtility.MATLAB); // dummy fileInfo
                                    maskFileInfo2.setEndianess(endianess);
                                    maskFileInfo2.setLevel5Format(level5Format);
                                    maskFileInfo2.setHeaderTextField(headerTextField);
                                    maskFileInfo2.setVersion(version);
                                    maskFileInfo2.setExtents(maskExtents);
                                    maskFileInfo2.setArrayName(arrayName);  
                                }
                            } // if (logicalFlag)
                            else { // !logicalFlag
                                imageExtents = new int[nDim+1];
                                imageLength = 1;
                                if (imagesFound == 1) {
                                    imageSlices = 1;
                                }
                                else {
                                    imageSlices2 = 1;
                                }
                                for (i = 0; i < nDim; i++) {
                                    if (i == 0) {
                                        imageExtents[1] = getInt(endianess);
                                        Preferences.debug("imageExtents[1] = " + imageExtents[1] + "\n", Preferences.DEBUG_FILEIO);
                                    }
                                    else if (i == 1) {
                                        imageExtents[0] = getInt(endianess);
                                        Preferences.debug("imageExtents[0] = " + imageExtents[0] + "\n", Preferences.DEBUG_FILEIO);
                                    }
                                    else {
                                        imageExtents[i] = getInt(endianess);
                                        Preferences.debug("imageExtents["+ i + "] = " + imageExtents[i] + "\n", 
                                                Preferences.DEBUG_FILEIO);
                                    }
                                    imageLength = imageLength * imageExtents[i];
                                    
                                    if (i > 1) {
                                        if (imagesFound == 1) {
                                            imageSlices = imageSlices * imageExtents[i];
                                        }
                                        else {
                                            imageSlices2 = imageSlices2 * imageExtents[i];
                                        }
                                    }
                                }
                                
                                
                                // Note that imageLength only includes slices in one field of a structure
                                imageExtents[nDim] = fieldNumber;
                                Preferences.debug("imageExtents[" + nDim + "] = " + imageExtents[nDim] + "\n", 
                                        Preferences.DEBUG_FILEIO);
                                if (nDim > 1) {
                                    if (imagesFound == 1) {
                                        imageSlices = imageSlices * fieldNumber;
                                    }
                                    else {
                                        imageSlices2 = imageSlices2 * fieldNumber;
                                    }
                                }
                                if (imagesFound == 1) {
                                    fileInfo.setExtents(imageExtents);
                                }
                                else {
                                    fileInfo2.setExtents(imageExtents);
                                }
                            } // else !logicalFlag
                            
                            if ((dimensionsArrayBytes % 8) != 0) {
                                // Skip over padding bytes
                                padBytes = 8 - (dimensionsArrayBytes % 8);
                                for (i = 0; i < padBytes; i++) {
                                    raFile.readByte();
                                }
                            } // if ((dimensionsArrayBytes % 8) != 0)
                            
                            numericArrayNameDataType = getInt(endianess);
                            if ((numericArrayNameDataType & 0xffff0000) != 0) {
                                // Small data element format    
                                numericArrayNameBytes = (numericArrayNameDataType & 0xffff0000) >>> 16;
                                numericArrayNameDataType = numericArrayNameDataType & 0xffff;
                                numericArrayName = getString(numericArrayNameBytes);
                                if (numericArrayNameBytes < 4) {
                                    for (i = 0; i < 4 - numericArrayNameBytes; i++) {
                                        // Skip over padding bytes
                                        raFile.readByte();
                                    }
                                }
                            }
                            else {
                                numericArrayNameBytes = getInt(endianess);
                                numericArrayName = getString(numericArrayNameBytes);
                                // Skip over padding bytes
                                if (numericArrayNameBytes > 0) {
                                    padBytes = 8 - (numericArrayNameBytes % 8);
                                    for (i = 0; i < padBytes; i++) {
                                       raFile.readByte();
                                    }
                                }
                            }
                            Preferences.debug("Numeric array name = " + numericArrayName + "\n", Preferences.DEBUG_FILEIO);
                        } // if (arrayClass == mxSTRUCT_CLASS)
                        realDataType = getInt(endianess);
                        if ((realDataType & 0xffff0000) != 0) {
                            // Small data element format    
                            realDataBytes = (realDataType & 0xffff0000) >>> 16;
                            realDataType = realDataType & 0xffff;
                            haveSmallRealData = true;
                        }
                        else {
                            realDataBytes = getInt(endianess);
                            haveSmallRealData = false;
                        }
                        if ((nDim >= 3) && (imageExtents[nDim - 1] == 3) && (!logicalFlag) && (!complexFlag) &&
                                ((realDataType == miUINT8) || (realDataType == miUINT16) || (realDataType == miDOUBLE)) &&
                                (nonLogicalField == 0)) {
                            newExtents = new int[nDim-1];
                            for (i = 0; i < nDim-1; i++) {
                                newExtents[i] = imageExtents[i];
                            }
                            imageExtents = new int[nDim-1];
                            for (i = 0; i < nDim-1; i++) {
                                imageExtents[i] = newExtents[i];
                            }
                            if (imagesFound == 1) {
                                imageSlices = imageSlices/3;
                                fileInfo.setExtents(imageExtents);
                            }
                            else {
                                imageSlices2 = imageSlices2/3;
                                fileInfo2.setExtents(imageExtents);
                            }
                            isColor = true;
                        }
                        else {
                            isColor = false;
                        }
                        sliceSize = imageExtents[0] * imageExtents[1];
                        if (imagesFound == 1) {
                        switch(realDataType) {
                        case miINT8:
                            Preferences.debug("Real data type = miINT8\n", Preferences.DEBUG_FILEIO);
                            Preferences.debug("Real data bytes = " + realDataBytes + "\n", Preferences.DEBUG_FILEIO);
                            if (logicalFlag) {
                                if (logicalFields == 1) {
                                    maskImage = new ModelImage(ModelStorageBase.BYTE, maskExtents, fileName + "_mask");
                                    maskFileInfo.setDataType(ModelStorageBase.BYTE);
                                    buffer = new byte[realDataBytes];
                                    raFile.read(buffer);
                                    tBuffer = new byte[buffer.length];
                                    numberSlices = buffer.length/sliceSize;
                                    j = 0;
                                    for (s = 0; s < numberSlices; s++) {
                                        for (x = 0; x < imageExtents[1]; x++) {
                                            for (y = 0; y < imageExtents[0]; y++) {
                                                tBuffer[j++] = buffer[x + imageExtents[1] * y + s * sliceSize];                                         
                                            }
                                        }
                                    }
                                    if (haveSmallRealData) {
                                        if (realDataBytes < 4) {
                                            padBytes = 4 - realDataBytes;
                                            for (i = 0; i < padBytes; i++) {
                                                raFile.readByte();
                                            }
                                        }
                                    }
                                    else if ((realDataBytes % 8) != 0) {
                                        padBytes = 8 - (realDataBytes % 8);
                                        for (i = 0; i < padBytes; i++) {
                                            raFile.readByte();
                                        }
                                    }  
                                    try {
                                        maskImage.importData(0, tBuffer, true);
                                    }
                                    catch(IOException e) {
                                       MipavUtil.displayError("IOException on maskImage.importData(0, tBuffer, true)");
                                       throw e;
                                    }
                                } // if (logicalFields == 1)
                            }
                            else if (!complexFlag) {
                                if (nonLogicalField == 0) {
                                    image = new ModelImage(ModelStorageBase.BYTE, imageExtents, fileName); 
                                    fileInfo.setDataType(ModelStorageBase.BYTE);
                                }
                                buffer = new byte[realDataBytes];
                                raFile.read(buffer);
                                tBuffer = new byte[buffer.length];
                                numberSlices = buffer.length/sliceSize;
                                j = 0;
                                for (s = 0; s < numberSlices; s++) {
                                    for (x = 0; x < imageExtents[1]; x++) {
                                        for (y = 0; y < imageExtents[0]; y++) {
                                            tBuffer[j++] = buffer[x + imageExtents[1] * y + s * sliceSize];                                         
                                        }
                                    }
                                }
                                if (haveSmallRealData) {
                                    if (realDataBytes < 4) {
                                        padBytes = 4 - realDataBytes;
                                        for (i = 0; i < padBytes; i++) {
                                            raFile.readByte();
                                        }
                                    }
                                }
                                else if ((realDataBytes % 8) != 0) {
                                    padBytes = 8 - (realDataBytes % 8);
                                    for (i = 0; i < padBytes; i++) {
                                        raFile.readByte();
                                    }
                                }  
                                try {
                                    image.importData(nonLogicalField * tBuffer.length, tBuffer, true);
                                }
                                catch(IOException e) {
                                   MipavUtil.displayError("IOException on image.importData(nonLogicalField* tBuffer.length, tBuffer, true)");
                                   throw e;
                                }
                            }
                            else {
                                if (nonLogicalField == 0) {
                                    image = new ModelImage(ModelStorageBase.COMPLEX, imageExtents, fileName); 
                                    fileInfo.setDataType(ModelStorageBase.COMPLEX);
                                }
                                buffer = new byte[realDataBytes];
                                raFile.read(buffer);
                                realBuffer = new float[realDataBytes];
                                numberSlices = buffer.length/sliceSize;
                                j = 0;
                                for (s = 0; s < numberSlices; s++) {
                                    for (x = 0; x < imageExtents[1]; x++) {
                                        for (y = 0; y < imageExtents[0]; y++) {
                                            realBuffer[j++] = (float)buffer[x + imageExtents[1] * y + s * sliceSize];                                           
                                        }
                                    }
                                }
                                
                                if (haveSmallRealData) {
                                    if (realDataBytes < 4) {
                                        padBytes = 4 - realDataBytes;
                                        for (i = 0; i < padBytes; i++) {
                                            raFile.readByte();
                                        }
                                    }
                                }
                                else if ((realDataBytes % 8) != 0) {
                                    padBytes = 8 - (realDataBytes % 8);
                                    for (i = 0; i < padBytes; i++) {
                                        raFile.readByte();
                                    }
                                }   
                                imaginaryDataType = getInt(endianess);
                                if ((imaginaryDataType & 0xffff0000) != 0) {
                                    // Small data element format    
                                    imaginaryDataBytes = (imaginaryDataType & 0xffff0000) >>> 16;
                                    imaginaryDataType = imaginaryDataType & 0xffff;
                                    haveSmallImaginaryData = true;
                                }
                                else {
                                    imaginaryDataBytes = getInt(endianess);
                                    haveSmallImaginaryData = false;
                                }
                                if (imaginaryDataType == miINT8) {
                                    Preferences.debug("imaginaryDataType == miINT8 as expected\n", Preferences.DEBUG_FILEIO);
                                }
                                else {
                                    Preferences.debug("imaginaryDataType unexpectedly == " + imaginaryDataType + "\n",
                                            Preferences.DEBUG_FILEIO);
                                }
              
                                if (imaginaryDataBytes == realDataBytes) {
                                    Preferences.debug("imaginaryDataBytes == realDataBytes as expected\n", 
                                            Preferences.DEBUG_FILEIO);
                                }
                                else {
                                    Preferences.debug("imaginaryDataBytes unexpectedly != realDataBytes\n", 
                                            Preferences.DEBUG_FILEIO);
                                }
                                raFile.read(buffer);
                                imaginaryBuffer = new float[imaginaryDataBytes];
                                j = 0;
                                for (s = 0; s < numberSlices; s++) {
                                    for (x = 0; x < imageExtents[1]; x++) {
                                        for (y = 0; y < imageExtents[0]; y++) {
                                            imaginaryBuffer[j++] = (float)buffer[x + imageExtents[1] * y + s * sliceSize];                                          
                                        }
                                    }
                                }
                                
                                if (haveSmallImaginaryData) {
                                    if (imaginaryDataBytes < 4) {
                                        padBytes = 4 - imaginaryDataBytes;
                                        for (i = 0; i < padBytes; i++) {
                                            raFile.readByte();
                                        }
                                    }
                                }
                                else if ((imaginaryDataBytes % 8) != 0) {
                                    padBytes = 8 - (imaginaryDataBytes % 8);
                                    for (i = 0; i < padBytes; i++) {
                                        raFile.readByte();
                                    }
                                }   
                                
                                try {
                                    image.importComplexData(2 * nonLogicalField* realBuffer.length, realBuffer, imaginaryBuffer, true, logMagDisplay);
                                }
                                catch(IOException e) {
                                   MipavUtil.displayError("IOException on image.importComplexData(2 * nonLogicalField * realBuffer.length," +
                                        "  realBuffer, imaginaryBuffer, true, logMagDisplay)");
                                   throw e;
                                }       
                            }
                            break;
                        case miUINT8:
                            Preferences.debug("Real data type = miUINT8\n", Preferences.DEBUG_FILEIO);
                            Preferences.debug("Real data bytes = " + realDataBytes + "\n", Preferences.DEBUG_FILEIO);
                            if ((field == 0) && (realDataBytes == 1)) {
                                buffer = new byte[realDataBytes];
                                raFile.read(buffer);
                                p.n = (short)(buffer[0] & 0xff);
                                if (haveSmallRealData) {
                                    if (realDataBytes < 4) {
                                        padBytes = 4 - realDataBytes;
                                        for (i = 0; i < padBytes; i++) {
                                            raFile.readByte();
                                        }
                                    }
                                }
                                else if ((realDataBytes % 8) != 0) {
                                    padBytes = 8 - (realDataBytes % 8);
                                    for (i = 0; i < padBytes; i++) {
                                        raFile.readByte();
                                    }
                                }  
                            }
                            else if (field == 3) {
                                buffer = new byte[realDataBytes];
                                raFile.read(buffer);
                                p.t = new short[realDataBytes];
                                for (j = 0; j < realDataBytes; j++) {
                                    p.t[j] = (short)(buffer[j] & 0xff);
                                }
                                if (haveSmallRealData) {
                                    if (realDataBytes < 4) {
                                        padBytes = 4 - realDataBytes;
                                        for (i = 0; i < padBytes; i++) {
                                            raFile.readByte();
                                        }
                                    }
                                }
                                else if ((realDataBytes % 8) != 0) {
                                    padBytes = 8 - (realDataBytes % 8);
                                    for (i = 0; i < padBytes; i++) {
                                        raFile.readByte();
                                    }
                                }      
                            }
                            else if (logicalFlag) {
                                if (logicalFields == 1) {
                                    maskImage = new ModelImage(ModelStorageBase.UBYTE, maskExtents, fileName + "_mask");
                                    maskFileInfo.setDataType(ModelStorageBase.UBYTE);
                                    buffer = new byte[realDataBytes];
                                    shortBuffer = new short[realDataBytes];
                                    raFile.read(buffer);
                                    numberSlices = buffer.length/sliceSize;
                                    j = 0;
                                    for (s = 0; s < numberSlices; s++) {
                                        for (x = 0; x < imageExtents[1]; x++) {
                                            for (y = 0; y < imageExtents[0]; y++) {
                                                shortBuffer[j++] = (short) (buffer[x + imageExtents[1] * y + s * sliceSize] & 0xff);                                            
                                            }
                                        }
                                    }
                                    
                                    if (haveSmallRealData) {
                                        if (realDataBytes < 4) {
                                            padBytes = 4 - realDataBytes;
                                            for (i = 0; i < padBytes; i++) {
                                                raFile.readByte();
                                            }
                                        }
                                    }
                                    else if ((realDataBytes % 8) != 0) {
                                        padBytes = 8 - (realDataBytes % 8);
                                        for (i = 0; i < padBytes; i++) {
                                            raFile.readByte();
                                        }
                                    }  
                                    try {
                                        maskImage.importUData(0, shortBuffer, true);
                                    }
                                    catch(IOException e) {
                                       MipavUtil.displayError("IOException on maskImage.importUData(0, shortBuffer, true)");
                                       throw e;
                                    }
                                } // if (logicalFields == 1)    
                            }
                            else if (isColor) {
                                if (nonLogicalField == 0) {
                                    image = new ModelImage(ModelStorageBase.ARGB, imageExtents, fileName);
                                    fileInfo.setDataType(ModelStorageBase.ARGB);
                                }
                                if ((realDataBytes % 3) == 0) {
                                    buffer = new byte[realDataBytes/3];
                                    tBuffer = new byte[buffer.length];
                                    numberSlices = buffer.length/sliceSize;
                                    for (i = 0; i < 3; i++) {
                                        raFile.read(buffer);
                                        j = 0;
                                        for (s = 0; s < numberSlices; s++) {
                                            for (x = 0; x < imageExtents[1]; x++) {
                                                for (y = 0; y < imageExtents[0]; y++) {
                                                    tBuffer[j++] = buffer[x + imageExtents[1] * y + s * sliceSize];                                         
                                                }
                                            }
                                        }
                                        
                                        try {
                                            image.importRGBData(i+1, nonLogicalField * tBuffer.length, tBuffer, true);
                                        }
                                        catch(IOException e) {
                                           MipavUtil.displayError("IOException on image.importRGBData(i," +
                                                " nonLogicalField * tBuffer.length, tBuffer, true)");
                                           throw e;
                                        }
                                    } // if (i = 0; i < 3; i++)
        
                                    
                                    if (haveSmallRealData) {
                                        if (realDataBytes < 4) {
                                            padBytes = 4 - realDataBytes;
                                            for (i = 0; i < padBytes; i++) {
                                                raFile.readByte();
                                            }
                                        }
                                    }
                                    else if ((realDataBytes % 8) != 0) {
                                        padBytes = 8 - (realDataBytes % 8);
                                        for (i = 0; i < padBytes; i++) {
                                            raFile.readByte();
                                        }
                                    } 
                                
                                } // if (realDataBytes % 3) == 0)
                            } // else if (isColor)
                            else if (!complexFlag) {
                                if (nonLogicalField == 0) {
                                    image = new ModelImage(ModelStorageBase.UBYTE, imageExtents, fileName);
                                    fileInfo.setDataType(ModelStorageBase.UBYTE);
                                }
                                buffer = new byte[realDataBytes];
                                raFile.read(buffer);
                                shortBuffer = new short[realDataBytes];
                                numberSlices = buffer.length/sliceSize;
                                j = 0;
                                for (s = 0; s < numberSlices; s++) {
                                    for (x = 0; x < imageExtents[1]; x++) {
                                        for (y = 0; y < imageExtents[0]; y++) {
                                            shortBuffer[j++] = (short) (buffer[x + imageExtents[1] * y + s * sliceSize] & 0xff);                                            
                                        }
                                    }
                                }
                                
                                if (haveSmallRealData) {
                                    if (realDataBytes < 4) {
                                        padBytes = 4 - realDataBytes;
                                        for (i = 0; i < padBytes; i++) {
                                            raFile.readByte();
                                        }
                                    }
                                }
                                else if ((realDataBytes % 8) != 0) {
                                    padBytes = 8 - (realDataBytes % 8);
                                    for (i = 0; i < padBytes; i++) {
                                        raFile.readByte();
                                    }
                                }  
                                try {
                                    image.importUData(nonLogicalField * shortBuffer.length, shortBuffer, true);
                                }
                                catch(IOException e) {
                                   MipavUtil.displayError("IOException on image.importData(nonLogicalField * shortBuffer.length, shortBuffer, true)");
                                   throw e;
                                }
                            }
                            else {
                                if (nonLogicalField == 0) {
                                    image = new ModelImage(ModelStorageBase.COMPLEX, imageExtents, fileName);
                                    fileInfo.setDataType(ModelStorageBase.COMPLEX);
                                }
                                buffer = new byte[realDataBytes];
                                raFile.read(buffer);
                                realBuffer = new float[realDataBytes];
                                numberSlices = buffer.length/sliceSize;
                                j = 0;
                                for (s = 0; s < numberSlices; s++) {
                                    for (x = 0; x < imageExtents[1]; x++) {
                                        for (y = 0; y < imageExtents[0]; y++) {
                                            realBuffer[j++] = (float) (buffer[x + imageExtents[1] * y + s * sliceSize] & 0xff);                                         
                                        }
                                    }
                                }
                                
                                if (haveSmallRealData) {
                                    if (realDataBytes < 4) {
                                        padBytes = 4 - realDataBytes;
                                        for (i = 0; i < padBytes; i++) {
                                            raFile.readByte();
                                        }
                                    }
                                }
                                else if ((realDataBytes % 8) != 0) {
                                    padBytes = 8 - (realDataBytes % 8);
                                    for (i = 0; i < padBytes; i++) {
                                        raFile.readByte();
                                    }
                                }  
                                imaginaryDataType = getInt(endianess);
                                if ((imaginaryDataType & 0xffff0000) != 0) {
                                    // Small data element format    
                                    imaginaryDataBytes = (imaginaryDataType & 0xffff0000) >>> 16;
                                    imaginaryDataType = imaginaryDataType & 0xffff;
                                    haveSmallImaginaryData = true;
                                }
                                else {
                                    imaginaryDataBytes = getInt(endianess);
                                    haveSmallImaginaryData = false;
                                }
                                if (imaginaryDataType == miUINT8) {
                                    Preferences.debug("imaginaryDataType == miUINT8 as expected\n", Preferences.DEBUG_FILEIO);
                                }
                                else {
                                    Preferences.debug("imaginaryDataType unexpectedly == " + imaginaryDataType + "\n", 
                                            Preferences.DEBUG_FILEIO);
                                }
                             
                                if (imaginaryDataBytes == realDataBytes) {
                                    Preferences.debug("imaginaryDataBytes == realDataBytes as expected\n", 
                                            Preferences.DEBUG_FILEIO);
                                }
                                else {
                                    Preferences.debug("imaginaryDataBytes unexpectedly != realDataBytes\n", 
                                            Preferences.DEBUG_FILEIO);
                                }
                                raFile.read(buffer);
                                imaginaryBuffer = new float[imaginaryDataBytes];
                                j = 0;
                                for (s = 0; s < numberSlices; s++) {
                                    for (x = 0; x < imageExtents[1]; x++) {
                                        for (y = 0; y < imageExtents[0]; y++) {
                                            imaginaryBuffer[j++] = (float) (buffer[x + imageExtents[1] * y + s * sliceSize] & 0xff);                                            
                                        }
                                    }
                                }
                                
                                if (haveSmallImaginaryData) {
                                    if (imaginaryDataBytes < 4) {
                                        padBytes = 4 - imaginaryDataBytes;
                                        for (i = 0; i < padBytes; i++) {
                                            raFile.readByte();
                                        }
                                    }
                                }
                                else if ((imaginaryDataBytes % 8) != 0) {
                                    padBytes = 8 - (imaginaryDataBytes % 8);
                                    for (i = 0; i < padBytes; i++) {
                                        raFile.readByte();
                                    }
                                }   
                                
                                try {
                                    image.importComplexData(2 * nonLogicalField * realBuffer.length, realBuffer, imaginaryBuffer, true, logMagDisplay);
                                }
                                catch(IOException e) {
                                   MipavUtil.displayError("IOException on image.importComplexData(2 * nonLogicalField * realBuffer.length," +
                                        "  realBuffer, imaginaryBuffer, true, logMagDisplay)");
                                   throw e;
                                }       
                            }
                            break;
                        case miINT16:
                            Preferences.debug("Real data type = miINT16\n", Preferences.DEBUG_FILEIO);
                            Preferences.debug("Real data bytes = " + realDataBytes + "\n", Preferences.DEBUG_FILEIO);
                            if (!complexFlag) {
                                if (nonLogicalField == 0) {
                                    image = new ModelImage(ModelStorageBase.SHORT, imageExtents, fileName); 
                                    fileInfo.setDataType(ModelStorageBase.SHORT);
                                }
                                buffer = new byte[realDataBytes];
                                raFile.read(buffer);
                                shortNumber = realDataBytes/2;
                                shortBuffer = new short[shortNumber];
                                numberSlices = shortNumber/sliceSize;
                                j = 0;
                                for (s = 0; s < numberSlices; s++) {
                                    for (x = 0; x < imageExtents[1]; x++) {
                                        for (y = 0; y < imageExtents[0]; y++) {
                                            index = 2*(x + imageExtents[1] * y + s * sliceSize);
                                            b1 = buffer[index] & 0xff;
                                            b2 = buffer[index+1] & 0xff;
                                            if (endianess == BIG_ENDIAN) {
                                                shortBuffer[j++] = (short)((b1 << 8) | b2); 
                                            }
                                            else {
                                                shortBuffer[j++] = (short)((b2 << 8) | b1);
                                            }
                                        }
                                    }
                                }
                                 
                                if (haveSmallRealData) {
                                    if (realDataBytes < 4) {
                                        padBytes = 4 - realDataBytes;
                                        for (i = 0; i < padBytes; i++) {
                                            raFile.readByte();
                                        }
                                    }
                                }
                                else if ((realDataBytes % 8) != 0) {
                                    padBytes = 8 - (realDataBytes % 8);
                                    for (i = 0; i < padBytes; i++) {
                                        raFile.readByte();
                                    }
                                }  
                                try {
                                    image.importData(nonLogicalField * shortBuffer.length, shortBuffer, true);
                                }
                                catch(IOException e) {
                                   MipavUtil.displayError("IOException on image.importData(nonLogicalField * shortBuffer.length, shortBuffer, true)");
                                   throw e;
                                }
                            }
                            else {
                                if (nonLogicalField == 0) {
                                    image = new ModelImage(ModelStorageBase.COMPLEX, imageExtents, fileName); 
                                    fileInfo.setDataType(ModelStorageBase.COMPLEX);
                                }
                                buffer = new byte[realDataBytes];
                                raFile.read(buffer);
                                shortNumber = realDataBytes/2;
                                realBuffer = new float[shortNumber];
                                numberSlices = shortNumber/sliceSize;
                                j = 0;
                                for (s = 0; s < numberSlices; s++) {
                                    for (x = 0; x < imageExtents[1]; x++) {
                                        for (y = 0; y < imageExtents[0]; y++) {
                                            index = 2*(x + imageExtents[1] * y + s * sliceSize);
                                            b1 = buffer[index] & 0xff;
                                            b2 = buffer[index+1] & 0xff;
                                            if (endianess == BIG_ENDIAN) {
                                                realBuffer[j++] = (float)((b1 << 8) | b2);  
                                            }
                                            else {
                                                realBuffer[j++] = (float)((b2 << 8) | b1);
                                            }
                                        }
                                    }
                                }
                                
                                if (haveSmallRealData) {
                                    if (realDataBytes < 4) {
                                        padBytes = 4 - realDataBytes;
                                        for (i = 0; i < padBytes; i++) {
                                            raFile.readByte();
                                        }
                                    }
                                }
                                else if ((realDataBytes % 8) != 0) {
                                    padBytes = 8 - (realDataBytes % 8);
                                    for (i = 0; i < padBytes; i++) {
                                        raFile.readByte();
                                    }
                                }    
                                imaginaryDataType = getInt(endianess);
                                if ((imaginaryDataType & 0xffff0000) != 0) {
                                    // Small data element format    
                                    imaginaryDataBytes = (imaginaryDataType & 0xffff0000) >>> 16;
                                    imaginaryDataType = imaginaryDataType & 0xffff;
                                    haveSmallImaginaryData = true;
                                }
                                else {
                                    imaginaryDataBytes = getInt(endianess);
                                    haveSmallImaginaryData = false;
                                }
                                if (imaginaryDataType == miINT16) {
                                    Preferences.debug("imaginaryDataType == miINT16 as expected\n", Preferences.DEBUG_FILEIO);
                                }
                                else {
                                    Preferences.debug("imaginaryDataType unexpectedly == " + imaginaryDataType + "\n", 
                                            Preferences.DEBUG_FILEIO);
                                }
                                
                                if (imaginaryDataBytes == realDataBytes) {
                                    Preferences.debug("imaginaryDataBytes == realDataBytes as expected\n", 
                                            Preferences.DEBUG_FILEIO);
                                }
                                else {
                                    Preferences.debug("imaginaryDataBytes unexpectedly != realDataBytes\n", 
                                            Preferences.DEBUG_FILEIO);
                                }
                                raFile.read(buffer);
                                imaginaryBuffer = new float[shortNumber];
                                j = 0;
                                for (s = 0; s < numberSlices; s++) {
                                    for (x = 0; x < imageExtents[1]; x++) {
                                        for (y = 0; y < imageExtents[0]; y++) {
                                            index = 2*(x + imageExtents[1] * y + s * sliceSize);
                                            b1 = buffer[index] & 0xff;
                                            b2 = buffer[index+1] & 0xff;
                                            if (endianess == BIG_ENDIAN) {
                                                imaginaryBuffer[j++] = (float)((b1 << 8) | b2); 
                                            }
                                            else {
                                                imaginaryBuffer[j++] = (float)((b2 << 8) | b1);
                                            }
                                        }
                                    }
                                }
                                
                                if (haveSmallImaginaryData) {
                                    if (imaginaryDataBytes < 4) {
                                        padBytes = 4 - imaginaryDataBytes;
                                        for (i = 0; i < padBytes; i++) {
                                            raFile.readByte();
                                        }
                                    }
                                }
                                else if ((imaginaryDataBytes % 8) != 0) {
                                    padBytes = 8 - (imaginaryDataBytes % 8);
                                    for (i = 0; i < padBytes; i++) {
                                        raFile.readByte();
                                    }
                                }   
                                
                                try {
                                    image.importComplexData(2 * nonLogicalField * realBuffer.length, realBuffer, imaginaryBuffer, true, logMagDisplay);
                                }
                                catch(IOException e) {
                                   MipavUtil.displayError("IOException on image.importComplexData(2 * nonLogicalField * realBuffer.length," +
                                        "  realBuffer, imaginaryBuffer, true, logMagDisplay)");
                                   throw e;
                                }       
                            }
                            break;
                        case miUINT16:
                            Preferences.debug("Real data type = miUINT16\n", Preferences.DEBUG_FILEIO);
                            Preferences.debug("Real data bytes = " + realDataBytes + "\n", Preferences.DEBUG_FILEIO);
                            if (isColor) {
                                if (nonLogicalField == 0) {
                                    image = new ModelImage(ModelStorageBase.ARGB_USHORT, imageExtents, fileName);
                                    fileInfo.setDataType(ModelStorageBase.ARGB_USHORT);
                                }
                                if ((realDataBytes % 3) == 0) {
                                    buffer = new byte[realDataBytes/3];
                                    shortNumber = realDataBytes/6;
                                    shortBuffer = new short[shortNumber];
                                    numberSlices = shortNumber/sliceSize;
                                    for (i = 0; i < 3; i++) {
                                        raFile.read(buffer);
                                        j = 0;
                                        for (s = 0; s < numberSlices; s++) {
                                            for (x = 0; x < imageExtents[1]; x++) {
                                                for (y = 0; y < imageExtents[0]; y++) {
                                                    index = 2*(x + imageExtents[1] * y + s * sliceSize);
                                                    b1 = buffer[index] & 0xff;
                                                    b2 = buffer[index+1] & 0xff;
                                                    if (endianess == BIG_ENDIAN) {
                                                        shortBuffer[j++] = (short) ((b1 << 8) | b2);    
                                                    }
                                                    else {
                                                        shortBuffer[j++] = (short) ((b2 << 8) | b1);
                                                    }
                                                }
                                            }
                                        }
                                        
                                        try {
                                            image.importRGBData(i+1, nonLogicalField * shortBuffer.length, shortBuffer, true);
                                        }
                                        catch(IOException e) {
                                           MipavUtil.displayError("IOException on image.importRGBData(i," +
                                                " nonLogicalField * shortBuffer.length, shortBuffer, true)");
                                           throw e;
                                        }
                                    } // if (i = 0; i < 3; i++)
        
                                    
                                    if (haveSmallRealData) {
                                        if (realDataBytes < 4) {
                                            padBytes = 4 - realDataBytes;
                                            for (i = 0; i < padBytes; i++) {
                                                raFile.readByte();
                                            }
                                        }
                                    }
                                    else if ((realDataBytes % 8) != 0) {
                                        padBytes = 8 - (realDataBytes % 8);
                                        for (i = 0; i < padBytes; i++) {
                                            raFile.readByte();
                                        }
                                    } 
                                
                                } // if (realDataBytes % 3) == 0)
                            } // if (isColor)
                            else if (!complexFlag) {
                                if (nonLogicalField == 0) {
                                    image = new ModelImage(ModelStorageBase.USHORT, imageExtents, fileName);
                                    fileInfo.setDataType(ModelStorageBase.USHORT);
                                }
                                buffer =  new byte[realDataBytes];
                                raFile.read(buffer);
                                shortNumber = realDataBytes/2;
                                intBuffer = new int[shortNumber];
                                numberSlices = shortNumber/sliceSize;
                                j = 0;
                                for (s = 0; s < numberSlices; s++) {
                                    for (x = 0; x < imageExtents[1]; x++) {
                                        for (y = 0; y < imageExtents[0]; y++) {
                                            index = 2*(x + imageExtents[1] * y + s * sliceSize);
                                            b1 = buffer[index] & 0xff;
                                            b2 = buffer[index+1] & 0xff;
                                            if (endianess == BIG_ENDIAN) {
                                                intBuffer[j++] = ((b1 << 8) | b2);  
                                            }
                                            else {
                                                intBuffer[j++] = ((b2 << 8) | b1);
                                            }
                                        }
                                    }
                                }
                                 
                                if (haveSmallRealData) {
                                    if (realDataBytes < 4) {
                                        padBytes = 4 - realDataBytes;
                                        for (i = 0; i < padBytes; i++) {
                                            raFile.readByte();
                                        }
                                    }
                                }
                                else if ((realDataBytes % 8) != 0) {
                                    padBytes = 8 - (realDataBytes % 8);
                                    for (i = 0; i < padBytes; i++) {
                                        raFile.readByte();
                                    }
                                }  
                                try {
                                    image.importUData(nonLogicalField * intBuffer.length, intBuffer, true);
                                }
                                catch(IOException e) {
                                   MipavUtil.displayError("IOException on image.importUData(nonLogicalField * intBuffer.length, intBuffer, true)");
                                   throw e;
                                }
                            }
                            else {
                                if (nonLogicalField == 0) {
                                    image = new ModelImage(ModelStorageBase.COMPLEX, imageExtents, fileName); 
                                    fileInfo.setDataType(ModelStorageBase.COMPLEX);
                                }
                                buffer = new byte[realDataBytes];
                                raFile.read(buffer);
                                shortNumber = realDataBytes/2;
                                realBuffer = new float[shortNumber];
                                numberSlices = shortNumber/sliceSize;
                                j = 0;
                                for (s = 0; s < numberSlices; s++) {
                                    for (x = 0; x < imageExtents[1]; x++) {
                                        for (y = 0; y < imageExtents[0]; y++) {
                                            index = 2*(x + imageExtents[1] * y + s * sliceSize);
                                            b1 = buffer[index] & 0xff;
                                            b2 = buffer[index+1] & 0xff;
                                            if (endianess == BIG_ENDIAN) {
                                                realBuffer[j++] = (float) ((b1 << 8) | b2); 
                                            }
                                            else {
                                                realBuffer[j++] = (float) ((b2 << 8) | b1);
                                            }
                                        }
                                    }
                                }
                                
                                if (haveSmallRealData) {
                                    if (realDataBytes < 4) {
                                        padBytes = 4 - realDataBytes;
                                        for (i = 0; i < padBytes; i++) {
                                            raFile.readByte();
                                        }
                                    }
                                }
                                else if ((realDataBytes % 8) != 0) {
                                    padBytes = 8 - (realDataBytes % 8);
                                    for (i = 0; i < padBytes; i++) {
                                        raFile.readByte();
                                    }
                                }  
                                imaginaryDataType = getInt(endianess);
                                if ((imaginaryDataType & 0xffff0000) != 0) {
                                    // Small data element format    
                                    imaginaryDataBytes = (imaginaryDataType & 0xffff0000) >>> 16;
                                    imaginaryDataType = imaginaryDataType & 0xffff;
                                    haveSmallImaginaryData = true;
                                }
                                else {
                                    imaginaryDataBytes = getInt(endianess);
                                    haveSmallImaginaryData = false;
                                }
                                if (imaginaryDataType == miUINT16) {
                                    Preferences.debug("imaginaryDataType == miUINT16 as expected\n", Preferences.DEBUG_FILEIO);
                                }
                                else {
                                    Preferences.debug("imaginaryDataType unexpectedly == " + imaginaryDataType + "\n", 
                                            Preferences.DEBUG_FILEIO);
                                }
                                
                                if (imaginaryDataBytes == realDataBytes) {
                                    Preferences.debug("imaginaryDataBytes == realDataBytes as expected\n", 
                                            Preferences.DEBUG_FILEIO);
                                }
                                else {
                                    Preferences.debug("imaginaryDataBytes unexpectedly != realDataBytes\n", 
                                            Preferences.DEBUG_FILEIO);
                                }
                                raFile.read(buffer);
                                imaginaryBuffer = new float[shortNumber];
                                j = 0;
                                for (s = 0; s < numberSlices; s++) {
                                    for (x = 0; x < imageExtents[1]; x++) {
                                        for (y = 0; y < imageExtents[0]; y++) {
                                            index = 2*(x + imageExtents[1] * y + s * sliceSize);
                                            b1 = buffer[index] & 0xff;
                                            b2 = buffer[index+1] & 0xff;
                                            if (endianess == BIG_ENDIAN) {
                                                imaginaryBuffer[j++] = (float) ((b1 << 8) | b2);    
                                            }
                                            else {
                                                imaginaryBuffer[j++] = (float) ((b2 << 8) | b1);
                                            }
                                        }
                                    }
                                }
                                
                                if (haveSmallImaginaryData) {
                                    if (imaginaryDataBytes < 4) {
                                        padBytes = 4 - imaginaryDataBytes;
                                        for (i = 0; i < padBytes; i++) {
                                            raFile.readByte();
                                        }
                                    }
                                }
                                else if ((imaginaryDataBytes % 8) != 0) {
                                    padBytes = 8 - (imaginaryDataBytes % 8);
                                    for (i = 0; i < padBytes; i++) {
                                        raFile.readByte();
                                    }
                                }   
                                
                                try {
                                    image.importComplexData(2 * nonLogicalField * realBuffer.length, realBuffer, imaginaryBuffer, true, logMagDisplay);
                                }
                                catch(IOException e) {
                                   MipavUtil.displayError("IOException on image.importComplexData(2 * nonLogicalField * realBuffer.length," +
                                        "  realBuffer, imaginaryBuffer, true, logMagDisplay)");
                                   throw e;
                                }       
                            }
                            break;
                        case miINT32:
                            Preferences.debug("Real data type = miINT32\n", Preferences.DEBUG_FILEIO);
                            Preferences.debug("Real data bytes = " + realDataBytes + "\n", Preferences.DEBUG_FILEIO);
                            if (!complexFlag) {
                                if (nonLogicalField == 0) {
                                    image = new ModelImage(ModelStorageBase.INTEGER, imageExtents, fileName);
                                    fileInfo.setDataType(ModelStorageBase.INTEGER);
                                }
                                buffer = new byte[realDataBytes];
                                raFile.read(buffer);
                                intNumber = realDataBytes/4;
                                intBuffer = new int[intNumber];
                                numberSlices = intNumber/sliceSize;
                                j = 0;
                                for (s = 0; s < numberSlices; s++) {
                                    for (x = 0; x < imageExtents[1]; x++) {
                                        for (y = 0; y < imageExtents[0]; y++) {
                                            index = 4*(x + imageExtents[1] * y + s * sliceSize);
                                            b1 = buffer[index] & 0xff;
                                            b2 = buffer[index+1] & 0xff;
                                            b3 = buffer[index+2] & 0xff;
                                            b4 = buffer[index+3] & 0xff;
                                            if (endianess == BIG_ENDIAN) {
                                                intBuffer[j++] = ((b1 << 24) | (b2 << 16) | (b3 << 8) | b4);
                                            }
                                            else {
                                                intBuffer[j++] = ((b4 << 24) | (b3 << 16) | (b2 << 8) | b1);
                                            }
                                        }
                                    }
                                }
                                
                                if (haveSmallRealData) {
                                    if (realDataBytes < 4) {
                                        padBytes = 4 - realDataBytes;
                                        for (i = 0; i < padBytes; i++) {
                                            raFile.readByte();
                                        }
                                    }
                                }
                                else if ((realDataBytes % 8) != 0) {
                                    padBytes = 8 - (realDataBytes % 8);
                                    for (i = 0; i < padBytes; i++) {
                                        raFile.readByte();
                                    }
                                }  
                                try {
                                    image.importData(nonLogicalField * intBuffer.length, intBuffer, true);
                                }
                                catch(IOException e) {
                                   MipavUtil.displayError("IOException on image.importData(nonLogicalField * intBuffer.length, intBuffer, true)");
                                   throw e;
                                }
                            }
                            else {
                                if (nonLogicalField == 0) {
                                    image = new ModelImage(ModelStorageBase.DCOMPLEX, imageExtents, fileName); 
                                    fileInfo.setDataType(ModelStorageBase.DCOMPLEX);
                                }
                                buffer = new byte[realDataBytes];
                                raFile.read(buffer);
                                intNumber = realDataBytes/4;
                                realDBuffer = new double[intNumber];
                                numberSlices = intNumber/sliceSize;
                                j = 0;
                                for (s = 0; s < numberSlices; s++) {
                                    for (x = 0; x < imageExtents[1]; x++) {
                                        for (y = 0; y < imageExtents[0]; y++) {
                                            index = 4*(x + imageExtents[1] * y + s * sliceSize);
                                            b1 = buffer[index] & 0xff;
                                            b2 = buffer[index+1] & 0xff;
                                            b3 = buffer[index+2] & 0xff;
                                            b4 = buffer[index+3] & 0xff;
                                            if (endianess == BIG_ENDIAN) {
                                                realDBuffer[j++] = (double)((b1 << 24) | (b2 << 16) | (b3 << 8) | b4);
                                            }
                                            else {
                                                realDBuffer[j++] = (double)((b4 << 24) | (b3 << 16) | (b2 << 8) | b1);
                                            }
                                        }
                                    }
                                }
                                
                                if (haveSmallRealData) {
                                    if (realDataBytes < 4) {
                                        padBytes = 4 - realDataBytes;
                                        for (i = 0; i < padBytes; i++) {
                                            raFile.readByte();
                                        }
                                    }
                                }
                                else if ((realDataBytes % 8) != 0) {
                                    padBytes = 8 - (realDataBytes % 8);
                                    for (i = 0; i < padBytes; i++) {
                                        raFile.readByte();
                                    }
                                }  
                                imaginaryDataType = getInt(endianess);
                                if ((imaginaryDataType & 0xffff0000) != 0) {
                                    // Small data element format    
                                    imaginaryDataBytes = (imaginaryDataType & 0xffff0000) >>> 16;
                                    imaginaryDataType = imaginaryDataType & 0xffff;
                                    haveSmallImaginaryData = true;
                                }
                                else {
                                    imaginaryDataBytes = getInt(endianess);
                                    haveSmallImaginaryData = false;
                                }
                                if (imaginaryDataType == miINT32) {
                                    Preferences.debug("imaginaryDataType == miINT32 as expected\n", Preferences.DEBUG_FILEIO);
                                }
                                else {
                                    Preferences.debug("imaginaryDataType unexpectedly == " + imaginaryDataType + "\n", 
                                            Preferences.DEBUG_FILEIO);
                                }
                                
                                if (imaginaryDataBytes == realDataBytes) {
                                    Preferences.debug("imaginaryDataBytes == realDataBytes as expected\n", 
                                            Preferences.DEBUG_FILEIO);
                                }
                                else {
                                    Preferences.debug("imaginaryDataBytes unexpectedly != realDataBytes\n", 
                                            Preferences.DEBUG_FILEIO);
                                }
                                raFile.read(buffer);
                                imaginaryDBuffer = new double[intNumber];
                                j = 0;
                                for (s = 0; s < numberSlices; s++) {
                                    for (x = 0; x < imageExtents[1]; x++) {
                                        for (y = 0; y < imageExtents[0]; y++) {
                                            index = 4*(x + imageExtents[1] * y + s * sliceSize);
                                            b1 = buffer[index] & 0xff;
                                            b2 = buffer[index+1] & 0xff;
                                            b3 = buffer[index+2] & 0xff;
                                            b4 = buffer[index+3] & 0xff;
                                            if (endianess == BIG_ENDIAN) {
                                                imaginaryDBuffer[j++] = (double)((b1 << 24) | (b2 << 16) | (b3 << 8) | b4);
                                            }
                                            else {
                                                imaginaryDBuffer[j++] = (double)((b4 << 24) | (b3 << 16) | (b2 << 8) | b1);
                                            }
                                        }
                                    }
                                }
                                
                                if (haveSmallImaginaryData) {
                                    if (imaginaryDataBytes < 4) {
                                        padBytes = 4 - imaginaryDataBytes;
                                        for (i = 0; i < padBytes; i++) {
                                            raFile.readByte();
                                        }
                                    }
                                }
                                else if ((imaginaryDataBytes % 8) != 0) {
                                    padBytes = 8 - (imaginaryDataBytes % 8);
                                    for (i = 0; i < padBytes; i++) {
                                        raFile.readByte();
                                    }
                                }   
                                
                                try {
                                    image.importDComplexData(2 * nonLogicalField * realDBuffer.length, realDBuffer, imaginaryDBuffer, true, logMagDisplay);
                                }
                                catch(IOException e) {
                                   MipavUtil.displayError("IOException on image.importDComplexData(2 * nonLogicalField * realDBuffer.length," +
                                        "  realDBuffer, imaginaryDBuffer, true, logMagDisplay)");
                                   throw e;
                                }       
                            }
                            break;
                        case miUINT32:
                            Preferences.debug("Real data type = miUINT32\n", Preferences.DEBUG_FILEIO);
                            Preferences.debug("Real data bytes = " + realDataBytes + "\n", Preferences.DEBUG_FILEIO);
                            if (!complexFlag) {
                                if (nonLogicalField == 0) {
                                    image = new ModelImage(ModelStorageBase.UINTEGER, imageExtents, fileName); 
                                    fileInfo.setDataType(ModelStorageBase.UINTEGER);
                                }
                                buffer = new byte[realDataBytes];
                                raFile.read(buffer);
                                intNumber = realDataBytes/4;
                                longBuffer = new long[intNumber];
                                numberSlices = intNumber/sliceSize;
                                j = 0;
                                for (s = 0; s < numberSlices; s++) {
                                    for (x = 0; x < imageExtents[1]; x++) {
                                        for (y = 0; y < imageExtents[0]; y++) {
                                            index = 4*(x + imageExtents[1] * y + s * sliceSize);
                                            b1L = buffer[index] & 0xffL;
                                            b2L = buffer[index+1] & 0xffL;
                                            b3L = buffer[index+2] & 0xffL;
                                            b4L = buffer[index+3] & 0xffL;
                                            if (endianess == BIG_ENDIAN) {
                                                longBuffer[j++] = ((b1L << 24) | (b2L << 16) | (b3L << 8) | b4L);
                                            }
                                            else {
                                                longBuffer[j++] = ((b4L << 24) | (b3L << 16) | (b2L << 8) | b1L);
                                            }
                                        }
                                    }
                                }
                                
                                if (haveSmallRealData) {
                                    if (realDataBytes < 4) {
                                        padBytes = 4 - realDataBytes;
                                        for (i = 0; i < padBytes; i++) {
                                            raFile.readByte();
                                        }
                                    }
                                }
                                else if ((realDataBytes % 8) != 0) {
                                    padBytes = 8 - (realDataBytes % 8);
                                    for (i = 0; i < padBytes; i++) {
                                        raFile.readByte();
                                    }
                                }  
                                try {
                                    image.importUData(nonLogicalField * longBuffer.length, longBuffer, true);
                                }
                                catch(IOException e) {
                                   MipavUtil.displayError("IOException on image.importUData(nonLogicalField * longBuffer.length, longBuffer, true)");
                                   throw e;
                                }
                            }
                            else {
                                if (nonLogicalField == 0) {
                                    image = new ModelImage(ModelStorageBase.DCOMPLEX, imageExtents, fileName); 
                                    fileInfo.setDataType(ModelStorageBase.DCOMPLEX);
                                }
                                buffer = new byte[realDataBytes];
                                raFile.read(buffer);
                                intNumber = realDataBytes/4;
                                realDBuffer = new double[intNumber];
                                numberSlices = intNumber/sliceSize;
                                j = 0;
                                for (s = 0; s < numberSlices; s++) {
                                    for (x = 0; x < imageExtents[1]; x++) {
                                        for (y = 0; y < imageExtents[0]; y++) {
                                            index = 4*(x + imageExtents[1] * y + s * sliceSize);
                                            b1L = buffer[index] & 0xffL;
                                            b2L = buffer[index+1] & 0xffL;
                                            b3L = buffer[index+2] & 0xffL;
                                            b4L = buffer[index+3] & 0xffL;
                                            if (endianess == BIG_ENDIAN) {
                                                realDBuffer[j++] = (double)((b1L << 24) | (b2L << 16) | (b3L << 8) | b4L);
                                            }
                                            else {
                                                realDBuffer[j++] = (double)((b4L << 24) | (b3L << 16) | (b2L << 8) | b1L);
                                            }
                                        }
                                    }
                                }
                                
                                if (haveSmallRealData) {
                                    if (realDataBytes < 4) {
                                        padBytes = 4 - realDataBytes;
                                        for (i = 0; i < padBytes; i++) {
                                            raFile.readByte();
                                        }
                                    }
                                }
                                else if ((realDataBytes % 8) != 0) {
                                    padBytes = 8 - (realDataBytes % 8);
                                    for (i = 0; i < padBytes; i++) {
                                        raFile.readByte();
                                    }
                                }   
                                imaginaryDataType = getInt(endianess);
                                if ((imaginaryDataType & 0xffff0000) != 0) {
                                    // Small data element format    
                                    imaginaryDataBytes = (imaginaryDataType & 0xffff0000) >>> 16;
                                    imaginaryDataType = imaginaryDataType & 0xffff;
                                    haveSmallImaginaryData = true;
                                }
                                else {
                                    imaginaryDataBytes = getInt(endianess);
                                    haveSmallImaginaryData = false;
                                }
                                if (imaginaryDataType == miUINT32) {
                                    Preferences.debug("imaginaryDataType == miUINT32 as expected\n", Preferences.DEBUG_FILEIO);
                                }
                                else {
                                    Preferences.debug("imaginaryDataType unexpectedly == " + imaginaryDataType + "\n", 
                                            Preferences.DEBUG_FILEIO);
                                }
                                
                                if (imaginaryDataBytes == realDataBytes) {
                                    Preferences.debug("imaginaryDataBytes == realDataBytes as expected\n", 
                                            Preferences.DEBUG_FILEIO);
                                }
                                else {
                                    Preferences.debug("imaginaryDataBytes unexpectedly != realDataBytes\n", 
                                            Preferences.DEBUG_FILEIO);
                                }
                                raFile.read(buffer);
                                imaginaryDBuffer = new double[intNumber];
                                j = 0;
                                for (s = 0; s < numberSlices; s++) {
                                    for (x = 0; x < imageExtents[1]; x++) {
                                        for (y = 0; y < imageExtents[0]; y++) {
                                            index = 4*(x + imageExtents[1] * y + s * sliceSize);
                                            b1L = buffer[index] & 0xffL;
                                            b2L = buffer[index+1] & 0xffL;
                                            b3L = buffer[index+2] & 0xffL;
                                            b4L = buffer[index+3] & 0xffL;
                                            if (endianess == BIG_ENDIAN) {
                                                imaginaryDBuffer[j++] = (double)((b1L << 24) | (b2L << 16) | (b3L << 8) | b4L);
                                            }
                                            else {
                                                imaginaryDBuffer[j++] = (double)((b4L << 24) | (b3L << 16) | (b2L << 8) | b1L);
                                            }
                                        }
                                    }
                                }
                                
                                if (haveSmallImaginaryData) {
                                    if (imaginaryDataBytes < 4) {
                                        padBytes = 4 - imaginaryDataBytes;
                                        for (i = 0; i < padBytes; i++) {
                                            raFile.readByte();
                                        }
                                    }
                                }
                                else if ((imaginaryDataBytes % 8) != 0) {
                                    padBytes = 8 - (imaginaryDataBytes % 8);
                                    for (i = 0; i < padBytes; i++) {
                                        raFile.readByte();
                                    }
                                }   
                                
                                try {
                                    image.importDComplexData(2 * nonLogicalField * realDBuffer.length, realDBuffer, imaginaryDBuffer, true, logMagDisplay);
                                }
                                catch(IOException e) {
                                   MipavUtil.displayError("IOException on image.importDComplexData(2 * nonLogicalField * realDBuffer.length," +
                                        "  realDBuffer, imaginaryDBuffer, true, logMagDisplay)");
                                   throw e;
                                }       
                            }
                            break;
                        case miSINGLE:
                            Preferences.debug("Real data type = miSINGLE\n", Preferences.DEBUG_FILEIO);
                            Preferences.debug("Real data bytes = " + realDataBytes + "\n", Preferences.DEBUG_FILEIO);
                            if (!complexFlag) {
                                if (nonLogicalField == 0) {
                                    image = new ModelImage(ModelStorageBase.FLOAT, imageExtents, fileName); 
                                    fileInfo.setDataType(ModelStorageBase.FLOAT);
                                }
                                buffer = new byte[realDataBytes];
                                raFile.read(buffer);
                                floatNumber = realDataBytes/4;
                                floatBuffer = new float[floatNumber];
                                numberSlices = floatNumber/sliceSize;
                                j = 0;
                                for (s = 0; s < numberSlices; s++) {
                                    for (x = 0; x < imageExtents[1]; x++) {
                                        for (y = 0; y < imageExtents[0]; y++) {
                                            index = 4*(x + imageExtents[1] * y + s * sliceSize);
                                            b1 = buffer[index] & 0xff;
                                            b2 = buffer[index+1] & 0xff;
                                            b3 = buffer[index+2] & 0xff;
                                            b4 = buffer[index+3] & 0xff;
                                            if (endianess == BIG_ENDIAN) {
                                                tmpInt = ((b1 << 24) | (b2 << 16) | (b3 << 8) | b4);
                                            }
                                            else {
                                                tmpInt = ((b4 << 24) | (b3 << 16) | (b2 << 8) | b1);
                                            }
                                            floatBuffer[j++] = Float.intBitsToFloat(tmpInt);
                                        }
                                    }
                                }
                                
                                if (haveSmallRealData) {
                                    if (realDataBytes < 4) {
                                        padBytes = 4 - realDataBytes;
                                        for (i = 0; i < padBytes; i++) {
                                            raFile.readByte();
                                        }
                                    }
                                }
                                else if ((realDataBytes % 8) != 0) {
                                    padBytes = 8 - (realDataBytes % 8);
                                    for (i = 0; i < padBytes; i++) {
                                        raFile.readByte();
                                    }
                                }  
                                try {
                                    image.importData(nonLogicalField * floatBuffer.length, floatBuffer, true);
                                }
                                catch(IOException e) {
                                   MipavUtil.displayError("IOException on image.importData(nonLogicalField * floatBuffer.length, floatBuffer, true)");
                                   throw e;
                                }
                            }
                            else {
                                if (nonLogicalField == 0) {
                                    image = new ModelImage(ModelStorageBase.COMPLEX, imageExtents, fileName); 
                                    fileInfo.setDataType(ModelStorageBase.COMPLEX);
                                }
                                buffer = new byte[realDataBytes];
                                raFile.read(buffer);
                                floatNumber = realDataBytes/4;
                                realBuffer = new float[floatNumber];
                                numberSlices = floatNumber/sliceSize;
                                j = 0;
                                for (s = 0; s < numberSlices; s++) {
                                    for (x = 0; x < imageExtents[1]; x++) {
                                        for (y = 0; y < imageExtents[0]; y++) {
                                            index = 4*(x + imageExtents[1] * y + s * sliceSize);
                                            b1 = buffer[index] & 0xff;
                                            b2 = buffer[index+1] & 0xff;
                                            b3 = buffer[index+2] & 0xff;
                                            b4 = buffer[index+3] & 0xff;
                                            if (endianess == BIG_ENDIAN) {
                                                tmpInt = ((b1 << 24) | (b2 << 16) | (b3 << 8) | b4);
                                            }
                                            else {
                                                tmpInt = ((b4 << 24) | (b3 << 16) | (b2 << 8) | b1);
                                            }
                                            realBuffer[j++] = Float.intBitsToFloat(tmpInt);
                                        }
                                    }
                                }
                                
                                if (haveSmallRealData) {
                                    if (realDataBytes < 4) {
                                        padBytes = 4 - realDataBytes;
                                        for (i = 0; i < padBytes; i++) {
                                            raFile.readByte();
                                        }
                                    }
                                }
                                else if ((realDataBytes % 8) != 0) {
                                    padBytes = 8 - (realDataBytes % 8);
                                    for (i = 0; i < padBytes; i++) {
                                        raFile.readByte();
                                    }
                                }    
                                imaginaryDataType = getInt(endianess);
                                if ((imaginaryDataType & 0xffff0000) != 0) {
                                    // Small data element format    
                                    imaginaryDataBytes = (imaginaryDataType & 0xffff0000) >>> 16;
                                    imaginaryDataType = imaginaryDataType & 0xffff;
                                    haveSmallImaginaryData = true;
                                }
                                else {
                                    imaginaryDataBytes = getInt(endianess);
                                    haveSmallImaginaryData = false;
                                }
                                if (imaginaryDataType == miSINGLE) {
                                    Preferences.debug("imaginaryDataType == miSINGLE as expected\n", Preferences.DEBUG_FILEIO);
                                }
                                else {
                                    Preferences.debug("imaginaryDataType unexpectedly == " + imaginaryDataType + "\n", 
                                            Preferences.DEBUG_FILEIO);
                                }
                               
                                if (imaginaryDataBytes == realDataBytes) {
                                    Preferences.debug("imaginaryDataBytes == realDataBytes as expected\n", 
                                            Preferences.DEBUG_FILEIO);
                                }
                                else {
                                    Preferences.debug("imaginaryDataBytes unexpectedly != realDataBytes\n", 
                                            Preferences.DEBUG_FILEIO);
                                }
                                raFile.read(buffer);
                                imaginaryBuffer = new float[floatNumber];
                                j = 0;
                                for (s = 0; s < numberSlices; s++) {
                                    for (x = 0; x < imageExtents[1]; x++) {
                                        for (y = 0; y < imageExtents[0]; y++) {
                                            index = 4*(x + imageExtents[1] * y + s * sliceSize);
                                            b1 = buffer[index] & 0xff;
                                            b2 = buffer[index+1] & 0xff;
                                            b3 = buffer[index+2] & 0xff;
                                            b4 = buffer[index+3] & 0xff;
                                            if (endianess == BIG_ENDIAN) {
                                                tmpInt = ((b1 << 24) | (b2 << 16) | (b3 << 8) | b4);
                                            }
                                            else {
                                                tmpInt = ((b4 << 24) | (b3 << 16) | (b2 << 8) | b1);
                                            }
                                            imaginaryBuffer[j++] = Float.intBitsToFloat(tmpInt);
                                        }
                                    }
                                }
                              
                                if (haveSmallImaginaryData) {
                                    if (imaginaryDataBytes < 4) {
                                        padBytes = 4 - imaginaryDataBytes;
                                        for (i = 0; i < padBytes; i++) {
                                            raFile.readByte();
                                        }
                                    }
                                }
                                else if ((imaginaryDataBytes % 8) != 0) {
                                    padBytes = 8 - (imaginaryDataBytes % 8);
                                    for (i = 0; i < padBytes; i++) {
                                        raFile.readByte();
                                    }
                                }   
                                
                                try {
                                    image.importComplexData(2 * nonLogicalField * realBuffer.length, realBuffer, imaginaryBuffer, true, logMagDisplay);
                                }
                                catch(IOException e) {
                                   MipavUtil.displayError("IOException on image.importComplexData(2 * nonLogicalField * realBuffer.length," +
                                        " realBuffer, imaginaryBuffer, true, logMagDisplay)");
                                   throw e;
                                }
                            }
                            break;
                        case miDOUBLE:
                            Preferences.debug("Real data type = miDOUBLE\n", Preferences.DEBUG_FILEIO);
                            Preferences.debug("Real data bytes = " + realDataBytes + "\n", Preferences.DEBUG_FILEIO);
                            if (field == 1) {
                                buffer = new byte[realDataBytes];
                                raFile.read(buffer);
                                doubleNumber = realDataBytes/8;
                                p.y = new double[doubleNumber];
                                j = 0;
                                for (s = 0; s < p.n; s++) {
                                    index = 8*s;
                                    b1L = buffer[index] & 0xffL;
                                    b2L = buffer[index+1] & 0xffL;
                                    b3L = buffer[index+2] & 0xffL;
                                    b4L = buffer[index+3] & 0xffL;
                                    b5L = buffer[index+4] & 0xffL;
                                    b6L = buffer[index+5] & 0xffL;
                                    b7L = buffer[index+6] & 0xffL;
                                    b8L = buffer[index+7] & 0xffL;
                                    if (endianess == BIG_ENDIAN) {
                                        tmpLong = ((b1L << 56) | (b2L << 48) | (b3L << 40) | (b4L << 32) |
                                                   (b5L << 24) | (b6L << 16) | (b7L << 8) | b8L);   
                                    }
                                    else {
                                        tmpLong = ((b8L << 56) | (b7L << 48) | (b6L << 40) | (b5L << 32) |
                                                   (b4L << 24) | (b3L << 16) | (b2L << 8) | b1L);
                                    }
                                    // The MATLAB spatial coordinate starts with (0.5, 0.5) at the upper left corner of the
                                    // upper left pixel and has (1.0, 1.0) at the center of the upper left pixel.
                                    // Subtract 1.0 to correspond to MIPAV.
                                    p.y[j++] = Double.longBitsToDouble(tmpLong) - 1.0; 
                                }
                                
                                if (haveSmallRealData) {
                                    if (realDataBytes < 4) {
                                        padBytes = 4 - realDataBytes;
                                        for (i = 0; i < padBytes; i++) {
                                            raFile.readByte();
                                        }
                                    }
                                }
                                else if ((realDataBytes % 8) != 0) {
                                    padBytes = 8 - (realDataBytes % 8);
                                    for (i = 0; i < padBytes; i++) {
                                        raFile.readByte();
                                    }
                                }      
                            }
                            else if (field == 2) {
                                buffer = new byte[realDataBytes];
                                raFile.read(buffer);
                                doubleNumber = realDataBytes/8;
                                p.x = new double[doubleNumber];
                                j = 0;
                                for (s = 0; s < p.n; s++) {
                                    index = 8*s;
                                    b1L = buffer[index] & 0xffL;
                                    b2L = buffer[index+1] & 0xffL;
                                    b3L = buffer[index+2] & 0xffL;
                                    b4L = buffer[index+3] & 0xffL;
                                    b5L = buffer[index+4] & 0xffL;
                                    b6L = buffer[index+5] & 0xffL;
                                    b7L = buffer[index+6] & 0xffL;
                                    b8L = buffer[index+7] & 0xffL;
                                    if (endianess == BIG_ENDIAN) {
                                        tmpLong = ((b1L << 56) | (b2L << 48) | (b3L << 40) | (b4L << 32) |
                                                   (b5L << 24) | (b6L << 16) | (b7L << 8) | b8L);   
                                    }
                                    else {
                                        tmpLong = ((b8L << 56) | (b7L << 48) | (b6L << 40) | (b5L << 32) |
                                                   (b4L << 24) | (b3L << 16) | (b2L << 8) | b1L);
                                    }
                                    // The MATLAB spatial coordinate starts with (0.5, 0.5) at the upper left corner of the
                                    // upper left pixel and has (1.0, 1.0) at the center of the upper left pixel.
                                    // Subtract 1.0 to correspond to MIPAV.
                                    p.x[j++] = Double.longBitsToDouble(tmpLong) - 1.0; 
                                }
                                
                                if (haveSmallRealData) {
                                    if (realDataBytes < 4) {
                                        padBytes = 4 - realDataBytes;
                                        for (i = 0; i < padBytes; i++) {
                                            raFile.readByte();
                                        }
                                    }
                                }
                                else if ((realDataBytes % 8) != 0) {
                                    padBytes = 8 - (realDataBytes % 8);
                                    for (i = 0; i < padBytes; i++) {
                                        raFile.readByte();
                                    }
                                }      
                            }
                            else if (field == 4) {
                                int pExtents[] = new int[2];
                                pExtents[0] = imageExtents[0];
                                pExtents[1] = imageExtents[1];
                                image = new ModelImage(ModelStorageBase.DOUBLE, pExtents, fileName);
                                p.I = image;
                                fileInfo.setDataType(ModelStorageBase.DOUBLE);
                                buffer = new byte[realDataBytes];
                                raFile.read(buffer);
                                doubleNumber = realDataBytes/8;
                                doubleBuffer = new double[doubleNumber];
                                j = 0;
                                for (x = 0; x < imageExtents[1]; x++) {
                                    for (y = 0; y < imageExtents[0]; y++) {
                                        index = 8*(x + imageExtents[1] * y);
                                        b1L = buffer[index] & 0xffL;
                                        b2L = buffer[index+1] & 0xffL;
                                        b3L = buffer[index+2] & 0xffL;
                                        b4L = buffer[index+3] & 0xffL;
                                        b5L = buffer[index+4] & 0xffL;
                                        b6L = buffer[index+5] & 0xffL;
                                        b7L = buffer[index+6] & 0xffL;
                                        b8L = buffer[index+7] & 0xffL;
                                        if (endianess == BIG_ENDIAN) {
                                            tmpLong = ((b1L << 56) | (b2L << 48) | (b3L << 40) | (b4L << 32) |
                                                       (b5L << 24) | (b6L << 16) | (b7L << 8) | b8L);   
                                        }
                                        else {
                                            tmpLong = ((b8L << 56) | (b7L << 48) | (b6L << 40) | (b5L << 32) |
                                                       (b4L << 24) | (b3L << 16) | (b2L << 8) | b1L);
                                        }
                                        doubleBuffer[j++] = Double.longBitsToDouble(tmpLong);
                                    }
                                }
                                
                                if (haveSmallRealData) {
                                    if (realDataBytes < 4) {
                                        padBytes = 4 - realDataBytes;
                                        for (i = 0; i < padBytes; i++) {
                                            raFile.readByte();
                                        }
                                    }
                                }
                                else if ((realDataBytes % 8) != 0) {
                                    padBytes = 8 - (realDataBytes % 8);
                                    for (i = 0; i < padBytes; i++) {
                                        raFile.readByte();
                                    }
                                }  
                                try {
                                    image.importData(0, doubleBuffer, true);
                                }
                                catch(IOException e) {
                                   MipavUtil.displayError("IOException on image.importData(0, doubleBuffer, true)");
                                   throw e;
                                }    
                            }
                            else if (isColor) {
                                if (nonLogicalField == 0) {
                                    image = new ModelImage(ModelStorageBase.ARGB_FLOAT, imageExtents, fileName);
                                    fileInfo.setDataType(ModelStorageBase.ARGB_FLOAT);
                                }
                                if ((realDataBytes % 3) == 0) {
                                    buffer = new byte[realDataBytes/3];
                                    doubleNumber = realDataBytes/24;
                                    floatBuffer = new float[doubleNumber];
                                    numberSlices = doubleNumber/sliceSize;
                                    for (i = 0; i < 3; i++) {
                                        raFile.read(buffer);
                                        j = 0;
                                        for (s = 0; s < numberSlices; s++) {
                                            for (x = 0; x < imageExtents[1]; x++) {
                                                for (y = 0; y < imageExtents[0]; y++) {
                                                    index = 8*(x + imageExtents[1] * y + s * sliceSize);
                                                    b1L = buffer[index] & 0xffL;
                                                    b2L = buffer[index+1] & 0xffL;
                                                    b3L = buffer[index+2] & 0xffL;
                                                    b4L = buffer[index+3] & 0xffL;
                                                    b5L = buffer[index+4] & 0xffL;
                                                    b6L = buffer[index+5] & 0xffL;
                                                    b7L = buffer[index+6] & 0xffL;
                                                    b8L = buffer[index+7] & 0xffL;
                                                    if (endianess == BIG_ENDIAN) {
                                                        tmpLong = ((b1L << 56) | (b2L << 48) | (b3L << 40) | (b4L << 32) |
                                                                   (b5L << 24) | (b6L << 16) | (b7L << 8) | b8L);   
                                                    }
                                                    else {
                                                        tmpLong = ((b8L << 56) | (b7L << 48) | (b6L << 40) | (b5L << 32) |
                                                                   (b4L << 24) | (b3L << 16) | (b2L << 8) | b1L);
                                                    }
                                                    floatBuffer[j++] = (float)(255.0 * Double.longBitsToDouble(tmpLong));
                                                }
                                            }
                                        }
                                        
                                        try {
                                            image.importRGBData(i+1, nonLogicalField * floatBuffer.length, floatBuffer, true);
                                        }
                                        catch(IOException e) {
                                           MipavUtil.displayError("IOException on image.importRGBData(i," +
                                                " nonLogicalField * floatBuffer.length, floatBuffer, true)");
                                           throw e;
                                        }
                                    } // if (i = 0; i < 3; i++)
        
                                    
                                    if (haveSmallRealData) {
                                        if (realDataBytes < 4) {
                                            padBytes = 4 - realDataBytes;
                                            for (i = 0; i < padBytes; i++) {
                                                raFile.readByte();
                                            }
                                        }
                                    }
                                    else if ((realDataBytes % 8) != 0) {
                                        padBytes = 8 - (realDataBytes % 8);
                                        for (i = 0; i < padBytes; i++) {
                                            raFile.readByte();
                                        }
                                    } 
                                
                                } // if (realDataBytes % 3) == 0)
                            } // else if (isColor)
                            else if (!complexFlag) {
                                if (nonLogicalField == 0) {
                                    image = new ModelImage(ModelStorageBase.DOUBLE, imageExtents, fileName); 
                                    fileInfo.setDataType(ModelStorageBase.DOUBLE);
                                }
                                buffer = new byte[realDataBytes];
                                raFile.read(buffer);
                                doubleNumber = realDataBytes/8;
                                doubleBuffer = new double[doubleNumber];
                                numberSlices = doubleNumber/sliceSize;
                                j = 0;
                                for (s = 0; s < numberSlices; s++) {
                                    for (x = 0; x < imageExtents[1]; x++) {
                                        for (y = 0; y < imageExtents[0]; y++) {
                                            index = 8*(x + imageExtents[1] * y + s * sliceSize);
                                            b1L = buffer[index] & 0xffL;
                                            b2L = buffer[index+1] & 0xffL;
                                            b3L = buffer[index+2] & 0xffL;
                                            b4L = buffer[index+3] & 0xffL;
                                            b5L = buffer[index+4] & 0xffL;
                                            b6L = buffer[index+5] & 0xffL;
                                            b7L = buffer[index+6] & 0xffL;
                                            b8L = buffer[index+7] & 0xffL;
                                            if (endianess == BIG_ENDIAN) {
                                                tmpLong = ((b1L << 56) | (b2L << 48) | (b3L << 40) | (b4L << 32) |
                                                           (b5L << 24) | (b6L << 16) | (b7L << 8) | b8L);   
                                            }
                                            else {
                                                tmpLong = ((b8L << 56) | (b7L << 48) | (b6L << 40) | (b5L << 32) |
                                                           (b4L << 24) | (b3L << 16) | (b2L << 8) | b1L);
                                            }
                                            doubleBuffer[j++] = Double.longBitsToDouble(tmpLong);
                                        }
                                    }
                                }
                                
                                if (haveSmallRealData) {
                                    if (realDataBytes < 4) {
                                        padBytes = 4 - realDataBytes;
                                        for (i = 0; i < padBytes; i++) {
                                            raFile.readByte();
                                        }
                                    }
                                }
                                else if ((realDataBytes % 8) != 0) {
                                    padBytes = 8 - (realDataBytes % 8);
                                    for (i = 0; i < padBytes; i++) {
                                        raFile.readByte();
                                    }
                                }  
                                try {
                                    image.importData(nonLogicalField * doubleBuffer.length, doubleBuffer, true);
                                }
                                catch(IOException e) {
                                   MipavUtil.displayError("IOException on image.importData(nonLogicalField * doubleBuffer.length, doubleBuffer, true)");
                                   throw e;
                                }
                            }
                            else {
                                if (nonLogicalField == 0) {
                                    image = new ModelImage(ModelStorageBase.DCOMPLEX, imageExtents, fileName); 
                                    fileInfo.setDataType(ModelStorageBase.DCOMPLEX);
                                }
                                buffer = new byte[realDataBytes];
                                raFile.read(buffer);
                                doubleNumber = realDataBytes/8;
                                realDBuffer = new double[doubleNumber];
                                numberSlices = doubleNumber/sliceSize;
                                j = 0;
                                for (s = 0; s < numberSlices; s++) {
                                    for (x = 0; x < imageExtents[1]; x++) {
                                        for (y = 0; y < imageExtents[0]; y++) {
                                            index = 8*(x + imageExtents[1] * y + s * sliceSize);
                                            b1L = buffer[index] & 0xffL;
                                            b2L = buffer[index+1] & 0xffL;
                                            b3L = buffer[index+2] & 0xffL;
                                            b4L = buffer[index+3] & 0xffL;
                                            b5L = buffer[index+4] & 0xffL;
                                            b6L = buffer[index+5] & 0xffL;
                                            b7L = buffer[index+6] & 0xffL;
                                            b8L = buffer[index+7] & 0xffL;
                                            if (endianess == BIG_ENDIAN) {
                                                tmpLong = ((b1L << 56) | (b2L << 48) | (b3L << 40) | (b4L << 32) |
                                                           (b5L << 24) | (b6L << 16) | (b7L << 8) | b8L);   
                                            }
                                            else {
                                                tmpLong = ((b8L << 56) | (b7L << 48) | (b6L << 40) | (b5L << 32) |
                                                           (b4L << 24) | (b3L << 16) | (b2L << 8) | b1L);
                                            }
                                            realDBuffer[j++] = Double.longBitsToDouble(tmpLong);
                                        }
                                    }
                                }
                                
                                if (haveSmallRealData) {
                                    if (realDataBytes < 4) {
                                        padBytes = 4 - realDataBytes;
                                        for (i = 0; i < padBytes; i++) {
                                            raFile.readByte();
                                        }
                                    }
                                }
                                else if ((realDataBytes % 8) != 0) {
                                    padBytes = 8 - (realDataBytes % 8);
                                    for (i = 0; i < padBytes; i++) {
                                        raFile.readByte();
                                    }
                                }   
                                imaginaryDataType = getInt(endianess);
                                if ((imaginaryDataType & 0xffff0000) != 0) {
                                    // Small data element format    
                                    imaginaryDataBytes = (imaginaryDataType & 0xffff0000) >>> 16;
                                    imaginaryDataType = imaginaryDataType & 0xffff;
                                    haveSmallImaginaryData = true;
                                }
                                else {
                                    imaginaryDataBytes = getInt(endianess);
                                    haveSmallImaginaryData = false;
                                }
                                if (imaginaryDataType == miDOUBLE) {
                                    Preferences.debug("imaginaryDataType == miDOUBLE as expected\n", Preferences.DEBUG_FILEIO);
                                }
                                else {
                                    Preferences.debug("imaginaryDataType unexpectedly == " + imaginaryDataType + "\n", 
                                            Preferences.DEBUG_FILEIO);
                                }
                                
                                if (imaginaryDataBytes == realDataBytes) {
                                    Preferences.debug("imaginaryDataBytes == realDataBytes as expected\n", 
                                            Preferences.DEBUG_FILEIO);
                                }
                                else {
                                    Preferences.debug("imaginaryDataBytes unexpectedly != realDataBytes\n", 
                                            Preferences.DEBUG_FILEIO);
                                }
                                raFile.read(buffer);
                                imaginaryDBuffer = new double[doubleNumber];
                                j = 0;
                                for (s = 0; s < numberSlices; s++) {
                                    for (x = 0; x < imageExtents[1]; x++) {
                                        for (y = 0; y < imageExtents[0]; y++) {
                                            index = 8*(x + imageExtents[1] * y + s * sliceSize);
                                            b1L = buffer[index] & 0xffL;
                                            b2L = buffer[index+1] & 0xffL;
                                            b3L = buffer[index+2] & 0xffL;
                                            b4L = buffer[index+3] & 0xffL;
                                            b5L = buffer[index+4] & 0xffL;
                                            b6L = buffer[index+5] & 0xffL;
                                            b7L = buffer[index+6] & 0xffL;
                                            b8L = buffer[index+7] & 0xffL;
                                            if (endianess == BIG_ENDIAN) {
                                                tmpLong = ((b1L << 56) | (b2L << 48) | (b3L << 40) | (b4L << 32) |
                                                           (b5L << 24) | (b6L << 16) | (b7L << 8) | b8L);   
                                            }
                                            else {
                                                tmpLong = ((b8L << 56) | (b7L << 48) | (b6L << 40) | (b5L << 32) |
                                                           (b4L << 24) | (b3L << 16) | (b2L << 8) | b1L);
                                            }
                                            imaginaryDBuffer[j++] = Double.longBitsToDouble(tmpLong);
                                        }
                                    }
                                }
                                
                                if (haveSmallImaginaryData) {
                                    if (imaginaryDataBytes < 4) {
                                        padBytes = 4 - imaginaryDataBytes;
                                        for (i = 0; i < padBytes; i++) {
                                            raFile.readByte();
                                        }
                                    }
                                }
                                else if ((imaginaryDataBytes % 8) != 0) {
                                    padBytes = 8 - (imaginaryDataBytes % 8);
                                    for (i = 0; i < padBytes; i++) {
                                        raFile.readByte();
                                    }
                                }   
                                
                                try {
                                    image.importDComplexData(2 * nonLogicalField * realDBuffer.length, realDBuffer, imaginaryDBuffer, true, logMagDisplay);
                                }
                                catch(IOException e) {
                                   MipavUtil.displayError("IOException on image.importDComplexData(2 * nonLogicalField * realDBuffer.length," +
                                        " realDBuffer, imaginaryDBuffer, true, logMagDisplay)");
                                   throw e;
                                }
                            }
                            break;
                        case miINT64:
                            Preferences.debug("Real data type = miINT64\n", Preferences.DEBUG_FILEIO);
                            Preferences.debug("Real data bytes = " + realDataBytes + "\n", Preferences.DEBUG_FILEIO);
                            if (!complexFlag) {
                                if (nonLogicalField == 0) {
                                    image = new ModelImage(ModelStorageBase.LONG, imageExtents, fileName);
                                    fileInfo.setDataType(ModelStorageBase.LONG);
                                }
                                buffer = new byte[realDataBytes];
                                raFile.read(buffer);
                                longNumber = realDataBytes/8;
                                longBuffer = new long[longNumber];
                                numberSlices = longNumber/sliceSize;
                                j = 0;
                                for (s = 0; s < numberSlices; s++) {
                                    for (x = 0; x < imageExtents[1]; x++) {
                                        for (y = 0; y < imageExtents[0]; y++) {
                                            index = 8*(x + imageExtents[1] * y + s * sliceSize);
                                            b1L = buffer[index] & 0xffL;
                                            b2L = buffer[index+1] & 0xffL;
                                            b3L = buffer[index+2] & 0xffL;
                                            b4L = buffer[index+3] & 0xffL;
                                            b5L = buffer[index+4] & 0xffL;
                                            b6L = buffer[index+5] & 0xffL;
                                            b7L = buffer[index+6] & 0xffL;
                                            b8L = buffer[index+7] & 0xffL;
                                            if (endianess == BIG_ENDIAN) {
                                                longBuffer[j++] = ((b1L << 56) | (b2L << 48) | (b3L << 40) | (b4L << 32) |
                                                           (b5L << 24) | (b6L << 16) | (b7L << 8) | b8L);   
                                            }
                                            else {
                                                longBuffer[j++] = ((b8L << 56) | (b7L << 48) | (b6L << 40) | (b5L << 32) |
                                                           (b4L << 24) | (b3L << 16) | (b2L << 8) | b1L);
                                            }
                                        }
                                    }
                                }
                                
                                if (haveSmallRealData) {
                                    if (realDataBytes < 4) {
                                        padBytes = 4 - realDataBytes;
                                        for (i = 0; i < padBytes; i++) {
                                            raFile.readByte();
                                        }
                                    }
                                }
                                else if ((realDataBytes % 8) != 0) {
                                    padBytes = 8 - (realDataBytes % 8);
                                    for (i = 0; i < padBytes; i++) {
                                        raFile.readByte();
                                    }
                                }  
                                try {
                                    image.importData(nonLogicalField * longBuffer.length, longBuffer, true);
                                }
                                catch(IOException e) {
                                   MipavUtil.displayError("IOException on image.importData(nonLogicalField * longBuffer.length, longBuffer, true)");
                                   throw e;
                                }
                            }
                            else {
                                if (nonLogicalField == 0) {
                                    image = new ModelImage(ModelStorageBase.DCOMPLEX, imageExtents, fileName); 
                                    fileInfo.setDataType(ModelStorageBase.DCOMPLEX);
                                }
                                buffer = new byte[realDataBytes];
                                raFile.read(buffer);
                                longNumber = realDataBytes/8;
                                realDBuffer = new double[longNumber];
                                numberSlices = longNumber/sliceSize;
                                j = 0;
                                for (s = 0; s < numberSlices; s++) {
                                    for (x = 0; x < imageExtents[1]; x++) {
                                        for (y = 0; y < imageExtents[0]; y++) {
                                            index = 8*(x + imageExtents[1] * y + s * sliceSize);
                                            b1L = buffer[index] & 0xffL;
                                            b2L = buffer[index+1] & 0xffL;
                                            b3L = buffer[index+2] & 0xffL;
                                            b4L = buffer[index+3] & 0xffL;
                                            b5L = buffer[index+4] & 0xffL;
                                            b6L = buffer[index+5] & 0xffL;
                                            b7L = buffer[index+6] & 0xffL;
                                            b8L = buffer[index+7] & 0xffL;
                                            if (endianess == BIG_ENDIAN) {
                                                realDBuffer[j++] = (double)((b1L << 56) | (b2L << 48) | (b3L << 40) | (b4L << 32) |
                                                           (b5L << 24) | (b6L << 16) | (b7L << 8) | b8L);   
                                            }
                                            else {
                                                realDBuffer[j++] = (double)((b8L << 56) | (b7L << 48) | (b6L << 40) | (b5L << 32) |
                                                           (b4L << 24) | (b3L << 16) | (b2L << 8) | b1L);
                                            }
                                        }
                                    }
                                }
                                
                                if (haveSmallRealData) {
                                    if (realDataBytes < 4) {
                                        padBytes = 4 - realDataBytes;
                                        for (i = 0; i < padBytes; i++) {
                                            raFile.readByte();
                                        }
                                    }
                                }
                                else if ((realDataBytes % 8) != 0) {
                                    padBytes = 8 - (realDataBytes % 8);
                                    for (i = 0; i < padBytes; i++) {
                                        raFile.readByte();
                                    }
                                }    
                                imaginaryDataType = getInt(endianess);
                                if ((imaginaryDataType & 0xffff0000) != 0) {
                                    // Small data element format    
                                    imaginaryDataBytes = (imaginaryDataType & 0xffff0000) >>> 16;
                                    imaginaryDataType = imaginaryDataType & 0xffff;
                                    haveSmallImaginaryData = true;
                                }
                                else {
                                    imaginaryDataBytes = getInt(endianess);
                                    haveSmallImaginaryData = false;
                                }
                                if (imaginaryDataType == miINT64) {
                                    Preferences.debug("imaginaryDataType == miINT64 as expected\n", Preferences.DEBUG_FILEIO);
                                }
                                else {
                                    Preferences.debug("imaginaryDataType unexpectedly == " + imaginaryDataType + "\n", 
                                            Preferences.DEBUG_FILEIO);
                                }
                                
                                if (imaginaryDataBytes == realDataBytes) {
                                    Preferences.debug("imaginaryDataBytes == realDataBytes as expected\n", 
                                            Preferences.DEBUG_FILEIO);
                                }
                                else {
                                    Preferences.debug("imaginaryDataBytes unexpectedly != realDataBytes\n", 
                                            Preferences.DEBUG_FILEIO);
                                }
                                raFile.read(buffer);
                                imaginaryDBuffer = new double[longNumber];
                                j = 0;
                                for (s = 0; s < numberSlices; s++) {
                                    for (x = 0; x < imageExtents[1]; x++) {
                                        for (y = 0; y < imageExtents[0]; y++) {
                                            index = 8*(x + imageExtents[1] * y + s * sliceSize);
                                            b1L = buffer[index] & 0xffL;
                                            b2L = buffer[index+1] & 0xffL;
                                            b3L = buffer[index+2] & 0xffL;
                                            b4L = buffer[index+3] & 0xffL;
                                            b5L = buffer[index+4] & 0xffL;
                                            b6L = buffer[index+5] & 0xffL;
                                            b7L = buffer[index+6] & 0xffL;
                                            b8L = buffer[index+7] & 0xffL;
                                            if (endianess == BIG_ENDIAN) {
                                                imaginaryDBuffer[j++] = (double)((b1L << 56) | (b2L << 48) | (b3L << 40) | (b4L << 32) |
                                                           (b5L << 24) | (b6L << 16) | (b7L << 8) | b8L);   
                                            }
                                            else {
                                                imaginaryDBuffer[j++] = (double)((b8L << 56) | (b7L << 48) | (b6L << 40) | (b5L << 32) |
                                                           (b4L << 24) | (b3L << 16) | (b2L << 8) | b1L);
                                            }
                                        }
                                    }
                                }
                                
                                if (haveSmallImaginaryData) {
                                    if (imaginaryDataBytes < 4) {
                                        padBytes = 4 - imaginaryDataBytes;
                                        for (i = 0; i < padBytes; i++) {
                                            raFile.readByte();
                                        }
                                    }
                                }
                                else if ((imaginaryDataBytes % 8) != 0) {
                                    padBytes = 8 - (imaginaryDataBytes % 8);
                                    for (i = 0; i < padBytes; i++) {
                                        raFile.readByte();
                                    }
                                }   
                                
                                try {
                                    image.importDComplexData(2 * nonLogicalField * realDBuffer.length, realDBuffer, imaginaryDBuffer, true, logMagDisplay);
                                }
                                catch(IOException e) {
                                   MipavUtil.displayError("IOException on image.importDComplexData(2 * nonLogicalField * realDBuffer.length," +
                                        " realDBuffer, imaginaryDBuffer, true, logMagDisplay)");
                                   throw e;
                                }
                            }
                            break;
                        case miUINT64:
                            Preferences.debug("Real data type = miUINT64\n", Preferences.DEBUG_FILEIO);
                            Preferences.debug("Real data bytes = " + realDataBytes + "\n", Preferences.DEBUG_FILEIO);
                            if (!complexFlag) {
                                if (nonLogicalField == 0) {
                                    image = new ModelImage(ModelStorageBase.LONG, imageExtents, fileName);
                                    fileInfo.setDataType(ModelStorageBase.LONG);
                                }
                                buffer = new byte[realDataBytes];
                                raFile.read(buffer);
                                longNumber = realDataBytes/8;
                                longBuffer = new long[longNumber];
                                numberSlices = longNumber/sliceSize;
                                j = 0;
                                for (s = 0; s < numberSlices; s++) {
                                    for (x = 0; x < imageExtents[1]; x++) {
                                        for (y = 0; y < imageExtents[0]; y++) {
                                            index = 8*(x + imageExtents[1] * y + s * sliceSize);
                                            b1L = buffer[index] & 0xffL;
                                            b2L = buffer[index+1] & 0xffL;
                                            b3L = buffer[index+2] & 0xffL;
                                            b4L = buffer[index+3] & 0xffL;
                                            b5L = buffer[index+4] & 0xffL;
                                            b6L = buffer[index+5] & 0xffL;
                                            b7L = buffer[index+6] & 0xffL;
                                            b8L = buffer[index+7] & 0xffL;
                                            if (endianess == BIG_ENDIAN) {
                                                longBuffer[j++] = ((b1L << 56) | (b2L << 48) | (b3L << 40) | (b4L << 32) |
                                                           (b5L << 24) | (b6L << 16) | (b7L << 8) | b8L);   
                                            }
                                            else {
                                                longBuffer[j++] = ((b8L << 56) | (b7L << 48) | (b6L << 40) | (b5L << 32) |
                                                           (b4L << 24) | (b3L << 16) | (b2L << 8) | b1L);
                                            }
                                        }
                                    }
                                }
                                
                                if (haveSmallRealData) {
                                    if (realDataBytes < 4) {
                                        padBytes = 4 - realDataBytes;
                                        for (i = 0; i < padBytes; i++) {
                                            raFile.readByte();
                                        }
                                    }
                                }
                                else if ((realDataBytes % 8) != 0) {
                                    padBytes = 8 - (realDataBytes % 8);
                                    for (i = 0; i < padBytes; i++) {
                                        raFile.readByte();
                                    }
                                }  
                                try {
                                    image.importData(nonLogicalField * longBuffer.length, longBuffer, true);
                                }
                                catch(IOException e) {
                                   MipavUtil.displayError("IOException on image.importData(nonLogicalField * longBuffer.length, longBuffer, true)");
                                   throw e;
                                }
                            }
                            else {
                                if (nonLogicalField == 0) {
                                    image = new ModelImage(ModelStorageBase.DCOMPLEX, imageExtents, fileName);
                                    fileInfo.setDataType(ModelStorageBase.DCOMPLEX);
                                }
                                buffer = new byte[realDataBytes];
                                raFile.read(buffer);
                                longNumber = realDataBytes/8;
                                realDBuffer = new double[longNumber];
                                numberSlices = longNumber/sliceSize;
                                j = 0;
                                for (s = 0; s < numberSlices; s++) {
                                    for (x = 0; x < imageExtents[1]; x++) {
                                        for (y = 0; y < imageExtents[0]; y++) {
                                            index = 8*(x + imageExtents[1] * y + s * sliceSize);
                                            b1L = buffer[index] & 0xffL;
                                            b2L = buffer[index+1] & 0xffL;
                                            b3L = buffer[index+2] & 0xffL;
                                            b4L = buffer[index+3] & 0xffL;
                                            b5L = buffer[index+4] & 0xffL;
                                            b6L = buffer[index+5] & 0xffL;
                                            b7L = buffer[index+6] & 0xffL;
                                            b8L = buffer[index+7] & 0xffL;
                                            if (endianess == BIG_ENDIAN) {
                                                realDBuffer[j++] = (double)((b1L << 56) | (b2L << 48) | (b3L << 40) | (b4L << 32) |
                                                           (b5L << 24) | (b6L << 16) | (b7L << 8) | b8L);   
                                            }
                                            else {
                                                realDBuffer[j++] = (double)((b8L << 56) | (b7L << 48) | (b6L << 40) | (b5L << 32) |
                                                           (b4L << 24) | (b3L << 16) | (b2L << 8) | b1L);
                                            }
                                        }
                                    }
                                }
                                
                                if (haveSmallRealData) {
                                    if (realDataBytes < 4) {
                                        padBytes = 4 - realDataBytes;
                                        for (i = 0; i < padBytes; i++) {
                                            raFile.readByte();
                                        }
                                    }
                                }
                                else if ((realDataBytes % 8) != 0) {
                                    padBytes = 8 - (realDataBytes % 8);
                                    for (i = 0; i < padBytes; i++) {
                                        raFile.readByte();
                                    }
                                }   
                                imaginaryDataType = getInt(endianess);
                                if ((imaginaryDataType & 0xffff0000) != 0) {
                                    // Small data element format    
                                    imaginaryDataBytes = (imaginaryDataType & 0xffff0000) >>> 16;
                                    imaginaryDataType = imaginaryDataType & 0xffff;
                                    haveSmallImaginaryData = true;
                                }
                                else {
                                    imaginaryDataBytes = getInt(endianess);
                                    haveSmallImaginaryData = false;
                                }
                                if (imaginaryDataType == miUINT64) {
                                    Preferences.debug("imaginaryDataType == miUINT64 as expected\n", Preferences.DEBUG_FILEIO);
                                }
                                else {
                                    Preferences.debug("imaginaryDataType unexpectedly == " + imaginaryDataType + "\n", 
                                            Preferences.DEBUG_FILEIO);
                                }
                                
                                if (imaginaryDataBytes == realDataBytes) {
                                    Preferences.debug("imaginaryDataBytes == realDataBytes as expected\n", 
                                            Preferences.DEBUG_FILEIO);
                                }
                                else {
                                    Preferences.debug("imaginaryDataBytes unexpectedly != realDataBytes\n", 
                                            Preferences.DEBUG_FILEIO);
                                }
                                raFile.read(buffer);
                                imaginaryDBuffer = new double[longNumber];
                                numberSlices = longNumber/sliceSize;
                                j = 0;
                                for (s = 0; s < numberSlices; s++) {
                                    for (x = 0; x < imageExtents[1]; x++) {
                                        for (y = 0; y < imageExtents[0]; y++) {
                                            index = 8*(x + imageExtents[1] * y + s * sliceSize);
                                            b1L = buffer[index] & 0xffL;
                                            b2L = buffer[index+1] & 0xffL;
                                            b3L = buffer[index+2] & 0xffL;
                                            b4L = buffer[index+3] & 0xffL;
                                            b5L = buffer[index+4] & 0xffL;
                                            b6L = buffer[index+5] & 0xffL;
                                            b7L = buffer[index+6] & 0xffL;
                                            b8L = buffer[index+7] & 0xffL;
                                            if (endianess == BIG_ENDIAN) {
                                                imaginaryDBuffer[j++] = (double)((b1L << 56) | (b2L << 48) | (b3L << 40) | (b4L << 32) |
                                                           (b5L << 24) | (b6L << 16) | (b7L << 8) | b8L);   
                                            }
                                            else {
                                                imaginaryDBuffer[j++] = (double)((b8L << 56) | (b7L << 48) | (b6L << 40) | (b5L << 32) |
                                                           (b4L << 24) | (b3L << 16) | (b2L << 8) | b1L);
                                            }
                                        }
                                    }
                                }
                                
                                if (haveSmallImaginaryData) {
                                    if (imaginaryDataBytes < 4) {
                                        padBytes = 4 - imaginaryDataBytes;
                                        for (i = 0; i < padBytes; i++) {
                                            raFile.readByte();
                                        }
                                    }
                                }
                                else if ((imaginaryDataBytes % 8) != 0) {
                                    padBytes = 8 - (imaginaryDataBytes % 8);
                                    for (i = 0; i < padBytes; i++) {
                                        raFile.readByte();
                                    }
                                }   
                                
                                try {
                                    image.importDComplexData(2 * nonLogicalField * realDBuffer.length, realDBuffer, imaginaryDBuffer, true, logMagDisplay);
                                }
                                catch(IOException e) {
                                   MipavUtil.displayError("IOException on image.importDComplexData(2 * nonLogicalField * realDBuffer.length, " +
                                        "realDBuffer, imaginaryDBuffer, true, logMagDisplay)");
                                   throw e;
                                }
                            }
                            break;
                        default:
                            Preferences.debug("Illegal data type = " + realDataType + "\n", Preferences.DEBUG_FILEIO);
                            Preferences.debug("Real data bytes = " + realDataBytes + "\n", Preferences.DEBUG_FILEIO);
                        }
                        
                        } // if (imagesFound == 1)
                        else { // imagesFound > 1
                            switch(realDataType) {
                            case miINT8:
                                Preferences.debug("Real data type = miINT8\n", Preferences.DEBUG_FILEIO);
                                Preferences.debug("Real data bytes = " + realDataBytes + "\n", Preferences.DEBUG_FILEIO);
                                if (logicalFlag) {
                                    if (logicalFields == 1) {
                                        maskImage2 = new ModelImage(ModelStorageBase.BYTE, maskExtents, fileName + "_mask");
                                        maskFileInfo2.setDataType(ModelStorageBase.BYTE);
                                        buffer = new byte[realDataBytes];
                                        raFile.read(buffer);
                                        tBuffer = new byte[buffer.length];
                                        numberSlices = buffer.length/sliceSize;
                                        j = 0;
                                        for (s = 0; s < numberSlices; s++) {
                                            for (x = 0; x < imageExtents[1]; x++) {
                                                for (y = 0; y < imageExtents[0]; y++) {
                                                    tBuffer[j++] = buffer[x + imageExtents[1] * y + s * sliceSize];                                         
                                                }
                                            }
                                        }
                                        if (haveSmallRealData) {
                                            if (realDataBytes < 4) {
                                                padBytes = 4 - realDataBytes;
                                                for (i = 0; i < padBytes; i++) {
                                                    raFile.readByte();
                                                }
                                            }
                                        }
                                        else if ((realDataBytes % 8) != 0) {
                                            padBytes = 8 - (realDataBytes % 8);
                                            for (i = 0; i < padBytes; i++) {
                                                raFile.readByte();
                                            }
                                        }  
                                        try {
                                            maskImage2.importData(0, tBuffer, true);
                                        }
                                        catch(IOException e) {
                                           MipavUtil.displayError("IOException on maskImage2.importData(0, tBuffer, true)");
                                           throw e;
                                        }
                                    } // if (logicalFields == 1)    
                                }
                                else if (!complexFlag) {
                                    if (nonLogicalField == 0) {
                                        image2 = new ModelImage(ModelStorageBase.BYTE, imageExtents, fileName);
                                        fileInfo2.setDataType(ModelStorageBase.BYTE);
                                    }
                                    buffer = new byte[realDataBytes];
                                    raFile.read(buffer);
                                    tBuffer = new byte[buffer.length];
                                    numberSlices = buffer.length/sliceSize;
                                    j = 0;
                                    for (s = 0; s < numberSlices; s++) {
                                        for (x = 0; x < imageExtents[1]; x++) {
                                            for (y = 0; y < imageExtents[0]; y++) {
                                                tBuffer[j++] = buffer[x + imageExtents[1] * y + s * sliceSize];                                         
                                            }
                                        }
                                    }
                                    if (haveSmallRealData) {
                                        if (realDataBytes < 4) {
                                            padBytes = 4 - realDataBytes;
                                            for (i = 0; i < padBytes; i++) {
                                                raFile.readByte();
                                            }
                                        }
                                    }
                                    else if ((realDataBytes % 8) != 0) {
                                        padBytes = 8 - (realDataBytes % 8);
                                        for (i = 0; i < padBytes; i++) {
                                            raFile.readByte();
                                        }
                                    }  
                                    try {
                                        image2.importData(nonLogicalField * tBuffer.length, tBuffer, true);
                                    }
                                    catch(IOException e) {
                                       MipavUtil.displayError("IOException on image2.importData(nonLogicalField * tBuffer.length, tBuffer, true)");
                                       throw e;
                                    }
                                }
                                else {
                                    if (nonLogicalField == 0) {
                                        image2 = new ModelImage(ModelStorageBase.COMPLEX, imageExtents, fileName); 
                                        fileInfo2.setDataType(ModelStorageBase.COMPLEX);
                                    }
                                    buffer = new byte[realDataBytes];
                                    raFile.read(buffer);
                                    realBuffer = new float[realDataBytes];
                                    numberSlices = buffer.length/sliceSize;
                                    j = 0;
                                    for (s = 0; s < numberSlices; s++) {
                                        for (x = 0; x < imageExtents[1]; x++) {
                                            for (y = 0; y < imageExtents[0]; y++) {
                                                realBuffer[j++] = (float)buffer[x + imageExtents[1] * y + s * sliceSize];                                           
                                            }
                                        }
                                    }
                                    
                                    if (haveSmallRealData) {
                                        if (realDataBytes < 4) {
                                            padBytes = 4 - realDataBytes;
                                            for (i = 0; i < padBytes; i++) {
                                                raFile.readByte();
                                            }
                                        }
                                    }
                                    else if ((realDataBytes % 8) != 0) {
                                        padBytes = 8 - (realDataBytes % 8);
                                        for (i = 0; i < padBytes; i++) {
                                            raFile.readByte();
                                        }
                                    }    
                                    imaginaryDataType = getInt(endianess);
                                    if ((imaginaryDataType & 0xffff0000) != 0) {
                                        // Small data element format    
                                        imaginaryDataBytes = (imaginaryDataType & 0xffff0000) >>> 16;
                                        imaginaryDataType = imaginaryDataType & 0xffff;
                                        haveSmallImaginaryData = true;
                                    }
                                    else {
                                        imaginaryDataBytes = getInt(endianess);
                                        haveSmallImaginaryData = false;
                                    }
                                    if (imaginaryDataType == miINT8) {
                                        Preferences.debug("imaginaryDataType == miINT8 as expected\n", Preferences.DEBUG_FILEIO);
                                    }
                                    else {
                                        Preferences.debug("imaginaryDataType unexpectedly == " + imaginaryDataType + "\n", 
                                                Preferences.DEBUG_FILEIO);
                                    }
                                   
                                    if (imaginaryDataBytes == realDataBytes) {
                                        Preferences.debug("imaginaryDataBytes == realDataBytes as expected\n", 
                                                Preferences.DEBUG_FILEIO);
                                    }
                                    else {
                                        Preferences.debug("imaginaryDataBytes unexpectedly != realDataBytes\n", 
                                                Preferences.DEBUG_FILEIO);
                                    }
                                    raFile.read(buffer);
                                    imaginaryBuffer = new float[imaginaryDataBytes];
                                    j = 0;
                                    for (s = 0; s < numberSlices; s++) {
                                        for (x = 0; x < imageExtents[1]; x++) {
                                            for (y = 0; y < imageExtents[0]; y++) {
                                                imaginaryBuffer[j++] = (float)buffer[x + imageExtents[1] * y + s * sliceSize];                                          
                                            }
                                        }
                                    }
                                    
                                    if (haveSmallImaginaryData) {
                                        if (imaginaryDataBytes < 4) {
                                            padBytes = 4 - imaginaryDataBytes;
                                            for (i = 0; i < padBytes; i++) {
                                                raFile.readByte();
                                            }
                                        }
                                    }
                                    else if ((imaginaryDataBytes % 8) != 0) {
                                        padBytes = 8 - (imaginaryDataBytes % 8);
                                        for (i = 0; i < padBytes; i++) {
                                            raFile.readByte();
                                        }
                                    }   
                                    
                                    try {
                                        image2.importComplexData(2 * nonLogicalField * realBuffer.length, realBuffer, imaginaryBuffer, true, logMagDisplay);
                                    }
                                    catch(IOException e) {
                                       MipavUtil.displayError("IOException on image2.importComplexData(2 * nonLogicalField * realBuffer.length, " +
                                            "realBuffer, imaginaryBuffer, true, logMagDisplay)");
                                       throw e;
                                    }       
                                }
                                break;
                            case miUINT8:
                                Preferences.debug("Real data type = miUINT8\n", Preferences.DEBUG_FILEIO);
                                Preferences.debug("Real data bytes = " + realDataBytes + "\n", Preferences.DEBUG_FILEIO);
                                if (logicalFlag) {
                                    if (logicalFields == 1) {
                                        maskImage2 = new ModelImage(ModelStorageBase.UBYTE, maskExtents, fileName + "_mask");
                                        maskFileInfo2.setDataType(ModelStorageBase.UBYTE);
                                        buffer = new byte[realDataBytes];
                                        shortBuffer = new short[realDataBytes];
                                        raFile.read(buffer);
                                        numberSlices = buffer.length/sliceSize;
                                        j = 0;
                                        for (s = 0; s < numberSlices; s++) {
                                            for (x = 0; x < imageExtents[1]; x++) {
                                                for (y = 0; y < imageExtents[0]; y++) {
                                                    shortBuffer[j++] = (short) (buffer[x + imageExtents[1] * y + s * sliceSize] & 0xff);                                            
                                                }
                                            }
                                        }
                                        
                                        if (haveSmallRealData) {
                                            if (realDataBytes < 4) {
                                                padBytes = 4 - realDataBytes;
                                                for (i = 0; i < padBytes; i++) {
                                                    raFile.readByte();
                                                }
                                            }
                                        }
                                        else if ((realDataBytes % 8) != 0) {
                                            padBytes = 8 - (realDataBytes % 8);
                                            for (i = 0; i < padBytes; i++) {
                                                raFile.readByte();
                                            }
                                        }  
                                        try {
                                            maskImage2.importUData(0, shortBuffer, true);
                                        }
                                        catch(IOException e) {
                                           MipavUtil.displayError("IOException on maskImage2.importUData(0, shortBuffer, true)");
                                           throw e;
                                        }
                                    } // if (logicalFields == 1)        
                                }
                                else if (isColor) {
                                    if (nonLogicalField == 0) {
                                        image2 = new ModelImage(ModelStorageBase.ARGB, imageExtents, fileName);
                                        fileInfo2.setDataType(ModelStorageBase.ARGB);
                                    }
                                    if ((realDataBytes % 3) == 0) {
                                        buffer = new byte[realDataBytes/3];
                                        tBuffer = new byte[buffer.length];
                                        numberSlices = buffer.length/sliceSize;
                                        for (i = 0; i < 3; i++) {
                                            raFile.read(buffer);
                                            j = 0;
                                            for (s = 0; s < numberSlices; s++) {
                                                for (x = 0; x < imageExtents[1]; x++) {
                                                    for (y = 0; y < imageExtents[0]; y++) {
                                                        tBuffer[j++] = buffer[x + imageExtents[1] * y + s * sliceSize];                                         
                                                    }
                                                }
                                            }
                                            
                                            try {
                                                image2.importRGBData(i+1, nonLogicalField * tBuffer.length, tBuffer, true);
                                            }
                                            catch(IOException e) {
                                               MipavUtil.displayError("IOException on image2.importRGBData(i," +
                                                    " nonLogicalField * tBuffer.length, tBuffer, true)");
                                               throw e;
                                            }
                                        } // if (i = 0; i < 3; i++)
            
                                        
                                        if (haveSmallRealData) {
                                            if (realDataBytes < 4) {
                                                padBytes = 4 - realDataBytes;
                                                for (i = 0; i < padBytes; i++) {
                                                    raFile.readByte();
                                                }
                                            }
                                        }
                                        else if ((realDataBytes % 8) != 0) {
                                            padBytes = 8 - (realDataBytes % 8);
                                            for (i = 0; i < padBytes; i++) {
                                                raFile.readByte();
                                            }
                                        } 
                                    
                                    } // if (realDataBytes % 3) == 0)
                                } // else if (isColor)
                                else if (!complexFlag) {
                                    if (nonLogicalField == 0) {
                                        image2 = new ModelImage(ModelStorageBase.UBYTE, imageExtents, fileName);
                                        fileInfo2.setDataType(ModelStorageBase.UBYTE);
                                    }
                                    buffer = new byte[realDataBytes];
                                    raFile.read(buffer);
                                    shortBuffer = new short[realDataBytes];
                                    numberSlices = buffer.length/sliceSize;
                                    j = 0;
                                    for (s = 0; s < numberSlices; s++) {
                                        for (x = 0; x < imageExtents[1]; x++) {
                                            for (y = 0; y < imageExtents[0]; y++) {
                                                shortBuffer[j++] = (short) (buffer[x + imageExtents[1] * y + s * sliceSize] & 0xff);                                            
                                            }
                                        }
                                    }
                                    
                                    if (haveSmallRealData) {
                                        if (realDataBytes < 4) {
                                            padBytes = 4 - realDataBytes;
                                            for (i = 0; i < padBytes; i++) {
                                                raFile.readByte();
                                            }
                                        }
                                    }
                                    else if ((realDataBytes % 8) != 0) {
                                        padBytes = 8 - (realDataBytes % 8);
                                        for (i = 0; i < padBytes; i++) {
                                            raFile.readByte();
                                        }
                                    }  
                                    try {
                                        image2.importUData(nonLogicalField * shortBuffer.length, shortBuffer, true);
                                    }
                                    catch(IOException e) {
                                       MipavUtil.displayError("IOException on image2.importData(nonLogicalField * shortBuffer.length, shortBuffer, true)");
                                       throw e;
                                    }
                                }
                                else {
                                    if (nonLogicalField == 0) {
                                        image2 = new ModelImage(ModelStorageBase.COMPLEX, imageExtents, fileName); 
                                        fileInfo2.setDataType(ModelStorageBase.COMPLEX);
                                    }
                                    buffer = new byte[realDataBytes];
                                    raFile.read(buffer);
                                    realBuffer = new float[realDataBytes];
                                    numberSlices = buffer.length/sliceSize;
                                    j = 0;
                                    for (s = 0; s < numberSlices; s++) {
                                        for (x = 0; x < imageExtents[1]; x++) {
                                            for (y = 0; y < imageExtents[0]; y++) {
                                                realBuffer[j++] = (float) (buffer[x + imageExtents[1] * y + s * sliceSize] & 0xff);                                         
                                            }
                                        }
                                    }
                                    
                                    if (haveSmallRealData) {
                                        if (realDataBytes < 4) {
                                            padBytes = 4 - realDataBytes;
                                            for (i = 0; i < padBytes; i++) {
                                                raFile.readByte();
                                            }
                                        }
                                    }
                                    else if ((realDataBytes % 8) != 0) {
                                        padBytes = 8 - (realDataBytes % 8);
                                        for (i = 0; i < padBytes; i++) {
                                            raFile.readByte();
                                        }
                                    }   
                                    imaginaryDataType = getInt(endianess);
                                    if ((imaginaryDataType & 0xffff0000) != 0) {
                                        // Small data element format    
                                        imaginaryDataBytes = (imaginaryDataType & 0xffff0000) >>> 16;
                                        imaginaryDataType = imaginaryDataType & 0xffff;
                                        haveSmallImaginaryData = true;
                                    }
                                    else {
                                        imaginaryDataBytes = getInt(endianess);
                                        haveSmallImaginaryData = false;
                                    }
                                    if (imaginaryDataType == miUINT8) {
                                        Preferences.debug("imaginaryDataType == miUINT8 as expected\n", Preferences.DEBUG_FILEIO);
                                    }
                                    else {
                                        Preferences.debug("imaginaryDataType unexpectedly == " + imaginaryDataType + "\n", 
                                                Preferences.DEBUG_FILEIO);
                                    }
                                    
                                    if (imaginaryDataBytes == realDataBytes) {
                                        Preferences.debug("imaginaryDataBytes == realDataBytes as expected\n", 
                                                Preferences.DEBUG_FILEIO);
                                    }
                                    else {
                                        Preferences.debug("imaginaryDataBytes unexpectedly != realDataBytes\n", 
                                                Preferences.DEBUG_FILEIO);
                                    }
                                    raFile.read(buffer);
                                    imaginaryBuffer = new float[imaginaryDataBytes];
                                    j = 0;
                                    for (s = 0; s < numberSlices; s++) {
                                        for (x = 0; x < imageExtents[1]; x++) {
                                            for (y = 0; y < imageExtents[0]; y++) {
                                                imaginaryBuffer[j++] = (float) (buffer[x + imageExtents[1] * y + s * sliceSize] & 0xff);                                            
                                            }
                                        }
                                    }
                                    
                                    if (haveSmallImaginaryData) {
                                        if (imaginaryDataBytes < 4) {
                                            padBytes = 4 - imaginaryDataBytes;
                                            for (i = 0; i < padBytes; i++) {
                                                raFile.readByte();
                                            }
                                        }
                                    }
                                    else if ((imaginaryDataBytes % 8) != 0) {
                                        padBytes = 8 - (imaginaryDataBytes % 8);
                                        for (i = 0; i < padBytes; i++) {
                                            raFile.readByte();
                                        }
                                    }   
                                    
                                    try {
                                        image2.importComplexData(2 * nonLogicalField * realBuffer.length, realBuffer, imaginaryBuffer, true, logMagDisplay);
                                    }
                                    catch(IOException e) {
                                       MipavUtil.displayError("IOException on image2.importComplexData(2 * nonLogicalField * realBuffer.length," +
                                            " realBuffer, imaginaryBuffer, true, logMagDisplay)");
                                       throw e;
                                    }   
                                }
                                break;
                            case miINT16:
                                Preferences.debug("Real data type = miINT16\n", Preferences.DEBUG_FILEIO);
                                Preferences.debug("Real data bytes = " + realDataBytes + "\n", Preferences.DEBUG_FILEIO);
                                if (!complexFlag) {
                                    if (nonLogicalField == 0) {
                                        image2 = new ModelImage(ModelStorageBase.SHORT, imageExtents, fileName); 
                                        fileInfo2.setDataType(ModelStorageBase.SHORT);
                                    }
                                    buffer = new byte[realDataBytes];
                                    raFile.read(buffer);
                                    shortNumber = realDataBytes/2;
                                    shortBuffer = new short[shortNumber];
                                    numberSlices = shortNumber/sliceSize;
                                    j = 0;
                                    for (s = 0; s < numberSlices; s++) {
                                        for (x = 0; x < imageExtents[1]; x++) {
                                            for (y = 0; y < imageExtents[0]; y++) {
                                                index = 2*(x + imageExtents[1] * y + s * sliceSize);
                                                b1 = buffer[index] & 0xff;
                                                b2 = buffer[index+1] & 0xff;
                                                if (endianess == BIG_ENDIAN) {
                                                    shortBuffer[j++] = (short)((b1 << 8) | b2); 
                                                }
                                                else {
                                                    shortBuffer[j++] = (short)((b2 << 8) | b1);
                                                }
                                            }
                                        }
                                    }
                                    
                                    if (haveSmallRealData) {
                                        if (realDataBytes < 4) {
                                            padBytes = 4 - realDataBytes;
                                            for (i = 0; i < padBytes; i++) {
                                                raFile.readByte();
                                            }
                                        }
                                    }
                                    else if ((realDataBytes % 8) != 0) {
                                        padBytes = 8 - (realDataBytes % 8);
                                        for (i = 0; i < padBytes; i++) {
                                            raFile.readByte();
                                        }
                                    }  
                                    try {
                                        image2.importData(nonLogicalField * shortBuffer.length, shortBuffer, true);
                                    }
                                    catch(IOException e) {
                                       MipavUtil.displayError("IOException on image2.importData(nonLogicalField * shortBuffer.length, shortBuffer, true)");
                                       throw e;
                                    }
                                }
                                else {
                                    if (nonLogicalField == 0) {
                                        image2 = new ModelImage(ModelStorageBase.COMPLEX, imageExtents, fileName); 
                                        fileInfo2.setDataType(ModelStorageBase.COMPLEX);
                                    }
                                    buffer = new byte[realDataBytes];
                                    raFile.read(buffer);
                                    shortNumber = realDataBytes/2;
                                    realBuffer = new float[shortNumber];
                                    numberSlices = shortNumber/sliceSize;
                                    j = 0;
                                    for (s = 0; s < numberSlices; s++) {
                                        for (x = 0; x < imageExtents[1]; x++) {
                                            for (y = 0; y < imageExtents[0]; y++) {
                                                index = 2*(x + imageExtents[1] * y + s * sliceSize);
                                                b1 = buffer[index] & 0xff;
                                                b2 = buffer[index+1] & 0xff;
                                                if (endianess == BIG_ENDIAN) {
                                                    realBuffer[j++] = (float)((b1 << 8) | b2);  
                                                }
                                                else {
                                                    realBuffer[j++] = (float)((b2 << 8) | b1);
                                                }
                                            }
                                        }
                                    }
                                    
                                    if (haveSmallRealData) {
                                        if (realDataBytes < 4) {
                                            padBytes = 4 - realDataBytes;
                                            for (i = 0; i < padBytes; i++) {
                                                raFile.readByte();
                                            }
                                        }
                                    }
                                    else if ((realDataBytes % 8) != 0) {
                                        padBytes = 8 - (realDataBytes % 8);
                                        for (i = 0; i < padBytes; i++) {
                                            raFile.readByte();
                                        }
                                    }  
                                    imaginaryDataType = getInt(endianess);
                                    if ((imaginaryDataType & 0xffff0000) != 0) {
                                        // Small data element format    
                                        imaginaryDataBytes = (imaginaryDataType & 0xffff0000) >>> 16;
                                        imaginaryDataType = imaginaryDataType & 0xffff;
                                        haveSmallImaginaryData = true;
                                    }
                                    else {
                                        imaginaryDataBytes = getInt(endianess);
                                        haveSmallImaginaryData = false;
                                    }
                                    if (imaginaryDataType == miINT16) {
                                        Preferences.debug("imaginaryDataType == miINT16 as expected\n", Preferences.DEBUG_FILEIO);
                                    }
                                    else {
                                        Preferences.debug("imaginaryDataType unexpectedly == " + imaginaryDataType + "\n", 
                                                Preferences.DEBUG_FILEIO);
                                    }
                                    
                                    if (imaginaryDataBytes == realDataBytes) {
                                        Preferences.debug("imaginaryDataBytes == realDataBytes as expected\n", 
                                                Preferences.DEBUG_FILEIO);
                                    }
                                    else {
                                        Preferences.debug("imaginaryDataBytes unexpectedly != realDataBytes\n", 
                                                Preferences.DEBUG_FILEIO);
                                    }
                                    raFile.read(buffer);
                                    imaginaryBuffer = new float[shortNumber];
                                    j = 0;
                                    for (s = 0; s < numberSlices; s++) {
                                        for (x = 0; x < imageExtents[1]; x++) {
                                            for (y = 0; y < imageExtents[0]; y++) {
                                                index = 2*(x + imageExtents[1] * y + s * sliceSize);
                                                b1 = buffer[index] & 0xff;
                                                b2 = buffer[index+1] & 0xff;
                                                if (endianess == BIG_ENDIAN) {
                                                    imaginaryBuffer[j++] = (float)((b1 << 8) | b2); 
                                                }
                                                else {
                                                    imaginaryBuffer[j++] = (float)((b2 << 8) | b1);
                                                }
                                            }
                                        }
                                    }
                                    
                                    if (haveSmallImaginaryData) {
                                        if (imaginaryDataBytes < 4) {
                                            padBytes = 4 - imaginaryDataBytes;
                                            for (i = 0; i < padBytes; i++) {
                                                raFile.readByte();
                                            }
                                        }
                                    }
                                    else if ((imaginaryDataBytes % 8) != 0) {
                                        padBytes = 8 - (imaginaryDataBytes % 8);
                                        for (i = 0; i < padBytes; i++) {
                                            raFile.readByte();
                                        }
                                    }   
                                    
                                    try {
                                        image2.importComplexData(2 * nonLogicalField * realBuffer.length, realBuffer, imaginaryBuffer, true, logMagDisplay);
                                    }
                                    catch(IOException e) {
                                       MipavUtil.displayError("IOException on image2.importComplexData(2 * nonLogicalField * realBuffer.length, " +
                                            "realBuffer, imaginaryBuffer, true, logMagDisplay)");
                                       throw e;
                                    }       
                                }
                                break;
                            case miUINT16:
                                Preferences.debug("Real data type = miUINT16\n", Preferences.DEBUG_FILEIO);
                                Preferences.debug("Real data bytes = " + realDataBytes + "\n", Preferences.DEBUG_FILEIO);
                                if (isColor) {
                                    if (nonLogicalField == 0) {
                                        image2 = new ModelImage(ModelStorageBase.ARGB_USHORT, imageExtents, fileName);
                                        fileInfo2.setDataType(ModelStorageBase.ARGB_USHORT);
                                    }
                                    if ((realDataBytes % 3) == 0) {
                                        buffer = new byte[realDataBytes/3];
                                        shortNumber = realDataBytes/6;
                                        shortBuffer = new short[shortNumber];
                                        numberSlices = shortNumber/sliceSize;
                                        for (i = 0; i < 3; i++) {
                                            raFile.read(buffer);
                                            j = 0;
                                            for (s = 0; s < numberSlices; s++) {
                                                for (x = 0; x < imageExtents[1]; x++) {
                                                    for (y = 0; y < imageExtents[0]; y++) {
                                                        index = 2*(x + imageExtents[1] * y + s * sliceSize);
                                                        b1 = buffer[index] & 0xff;
                                                        b2 = buffer[index+1] & 0xff;
                                                        if (endianess == BIG_ENDIAN) {
                                                            shortBuffer[j++] = (short) ((b1 << 8) | b2);    
                                                        }
                                                        else {
                                                            shortBuffer[j++] = (short) ((b2 << 8) | b1);
                                                        }
                                                    }
                                                }
                                            }
                                            
                                            try {
                                                image2.importRGBData(i+1, nonLogicalField * shortBuffer.length, shortBuffer, true);
                                            }
                                            catch(IOException e) {
                                               MipavUtil.displayError("IOException on image2.importRGBData(i," +
                                                    " nonLogicalField * shortBuffer.length, shortBuffer, true)");
                                               throw e;
                                            }
                                        } // if (i = 0; i < 3; i++)
            
                                        
                                        if (haveSmallRealData) {
                                            if (realDataBytes < 4) {
                                                padBytes = 4 - realDataBytes;
                                                for (i = 0; i < padBytes; i++) {
                                                    raFile.readByte();
                                                }
                                            }
                                        }
                                        else if ((realDataBytes % 8) != 0) {
                                            padBytes = 8 - (realDataBytes % 8);
                                            for (i = 0; i < padBytes; i++) {
                                                raFile.readByte();
                                            }
                                        } 
                                    
                                    } // if (realDataBytes % 3) == 0)
                                } // if (isColor)
                                else if (!complexFlag) {
                                    if (nonLogicalField == 0) {
                                        image2 = new ModelImage(ModelStorageBase.USHORT, imageExtents, fileName); 
                                        fileInfo2.setDataType(ModelStorageBase.USHORT);
                                    }
                                    buffer =  new byte[realDataBytes];
                                    raFile.read(buffer);
                                    shortNumber = realDataBytes/2;
                                    intBuffer = new int[shortNumber];
                                    numberSlices = shortNumber/sliceSize;
                                    j = 0;
                                    for (s = 0; s < numberSlices; s++) {
                                        for (x = 0; x < imageExtents[1]; x++) {
                                            for (y = 0; y < imageExtents[0]; y++) {
                                                index = 2*(x + imageExtents[1] * y + s * sliceSize);
                                                b1 = buffer[index] & 0xff;
                                                b2 = buffer[index+1] & 0xff;
                                                if (endianess == BIG_ENDIAN) {
                                                    intBuffer[j++] = ((b1 << 8) | b2);  
                                                }
                                                else {
                                                    intBuffer[j++] = ((b2 << 8) | b1);
                                                }
                                            }
                                        }
                                    }
                                    
                                    if (haveSmallRealData) {
                                        if (realDataBytes < 4) {
                                            padBytes = 4 - realDataBytes;
                                            for (i = 0; i < padBytes; i++) {
                                                raFile.readByte();
                                            }
                                        }
                                    }
                                    else if ((realDataBytes % 8) != 0) {
                                        padBytes = 8 - (realDataBytes % 8);
                                        for (i = 0; i < padBytes; i++) {
                                            raFile.readByte();
                                        }
                                    }  
                                    try {
                                        image2.importUData(nonLogicalField * intBuffer.length, intBuffer, true);
                                    }
                                    catch(IOException e) {
                                       MipavUtil.displayError("IOException on image2.importUData(nonLogicalField * intBuffer.length, intBuffer, true)");
                                       throw e;
                                    }
                                }
                                else {
                                    if (nonLogicalField == 0) {
                                        image2 = new ModelImage(ModelStorageBase.COMPLEX, imageExtents, fileName); 
                                        fileInfo2.setDataType(ModelStorageBase.COMPLEX);
                                    }
                                    buffer = new byte[realDataBytes];
                                    raFile.read(buffer);
                                    shortNumber = realDataBytes/2;
                                    realBuffer = new float[shortNumber];
                                    numberSlices = shortNumber/sliceSize;
                                    j = 0;
                                    for (s = 0; s < numberSlices; s++) {
                                        for (x = 0; x < imageExtents[1]; x++) {
                                            for (y = 0; y < imageExtents[0]; y++) {
                                                index = 2*(x + imageExtents[1] * y + s * sliceSize);
                                                b1 = buffer[index] & 0xff;
                                                b2 = buffer[index+1] & 0xff;
                                                if (endianess == BIG_ENDIAN) {
                                                    realBuffer[j++] = (float) ((b1 << 8) | b2); 
                                                }
                                                else {
                                                    realBuffer[j++] = (float) ((b2 << 8) | b1);
                                                }
                                            }
                                        }
                                    }
                                    
                                    if (haveSmallRealData) {
                                        if (realDataBytes < 4) {
                                            padBytes = 4 - realDataBytes;
                                            for (i = 0; i < padBytes; i++) {
                                                raFile.readByte();
                                            }
                                        }
                                    }
                                    else if ((realDataBytes % 8) != 0) {
                                        padBytes = 8 - (realDataBytes % 8);
                                        for (i = 0; i < padBytes; i++) {
                                            raFile.readByte();
                                        }
                                    }   
                                    imaginaryDataType = getInt(endianess);
                                    if ((imaginaryDataType & 0xffff0000) != 0) {
                                        // Small data element format    
                                        imaginaryDataBytes = (imaginaryDataType & 0xffff0000) >>> 16;
                                        imaginaryDataType = imaginaryDataType & 0xffff;
                                        haveSmallImaginaryData = true;
                                    }
                                    else {
                                        imaginaryDataBytes = getInt(endianess);
                                        haveSmallImaginaryData = false;
                                    }
                                    if (imaginaryDataType == miUINT16) {
                                        Preferences.debug("imaginaryDataType == miUINT16 as expected\n", Preferences.DEBUG_FILEIO);
                                    }
                                    else {
                                        Preferences.debug("imaginaryDataType unexpectedly == " + imaginaryDataType + "\n", 
                                                Preferences.DEBUG_FILEIO);
                                    }
                                   
                                    if (imaginaryDataBytes == realDataBytes) {
                                        Preferences.debug("imaginaryDataBytes == realDataBytes as expected\n", 
                                                Preferences.DEBUG_FILEIO);
                                    }
                                    else {
                                        Preferences.debug("imaginaryDataBytes unexpectedly != realDataBytes\n", 
                                                Preferences.DEBUG_FILEIO);
                                    }
                                    raFile.read(buffer);
                                    imaginaryBuffer = new float[shortNumber];
                                    j = 0;
                                    for (s = 0; s < numberSlices; s++) {
                                        for (x = 0; x < imageExtents[1]; x++) {
                                            for (y = 0; y < imageExtents[0]; y++) {
                                                index = 2*(x + imageExtents[1] * y + s * sliceSize);
                                                b1 = buffer[index] & 0xff;
                                                b2 = buffer[index+1] & 0xff;
                                                if (endianess == BIG_ENDIAN) {
                                                    imaginaryBuffer[j++] = (float) ((b1 << 8) | b2);    
                                                }
                                                else {
                                                    imaginaryBuffer[j++] = (float) ((b2 << 8) | b1);
                                                }
                                            }
                                        }
                                    }
                                    
                                    if (haveSmallImaginaryData) {
                                        if (imaginaryDataBytes < 4) {
                                            padBytes = 4 - imaginaryDataBytes;
                                            for (i = 0; i < padBytes; i++) {
                                                raFile.readByte();
                                            }
                                        }
                                    }
                                    else if ((imaginaryDataBytes % 8) != 0) {
                                        padBytes = 8 - (imaginaryDataBytes % 8);
                                        for (i = 0; i < padBytes; i++) {
                                            raFile.readByte();
                                        }
                                    }   
                                    
                                    try {
                                        image2.importComplexData(2 * nonLogicalField * realBuffer.length, realBuffer, imaginaryBuffer, true, logMagDisplay);
                                    }
                                    catch(IOException e) {
                                       MipavUtil.displayError("IOException on image2.importComplexData(2 * nonLogicalField * realBuffer.length," +
                                            " realBuffer, imaginaryBuffer, true, logMagDisplay)");
                                       throw e;
                                    }       
                                }
                                break;
                            case miINT32:
                                Preferences.debug("Real data type = miINT32\n", Preferences.DEBUG_FILEIO);
                                Preferences.debug("Real data bytes = " + realDataBytes + "\n", Preferences.DEBUG_FILEIO);
                                if (!complexFlag) {
                                    if (nonLogicalField == 0) {
                                        image2 = new ModelImage(ModelStorageBase.INTEGER, imageExtents, fileName);
                                        fileInfo2.setDataType(ModelStorageBase.INTEGER);
                                    }
                                    buffer = new byte[realDataBytes];
                                    raFile.read(buffer);
                                    intNumber = realDataBytes/4;
                                    intBuffer = new int[intNumber];
                                    numberSlices = intNumber/sliceSize;
                                    j = 0;
                                    for (s = 0; s < numberSlices; s++) {
                                        for (x = 0; x < imageExtents[1]; x++) {
                                            for (y = 0; y < imageExtents[0]; y++) {
                                                index = 4*(x + imageExtents[1] * y + s * sliceSize);
                                                b1 = buffer[index] & 0xff;
                                                b2 = buffer[index+1] & 0xff;
                                                b3 = buffer[index+2] & 0xff;
                                                b4 = buffer[index+3] & 0xff;
                                                if (endianess == BIG_ENDIAN) {
                                                    intBuffer[j++] = ((b1 << 24) | (b2 << 16) | (b3 << 8) | b4);
                                                }
                                                else {
                                                    intBuffer[j++] = ((b4 << 24) | (b3 << 16) | (b2 << 8) | b1);
                                                }
                                            }
                                        }
                                    }
                                    
                                    if (haveSmallRealData) {
                                        if (realDataBytes < 4) {
                                            padBytes = 4 - realDataBytes;
                                            for (i = 0; i < padBytes; i++) {
                                                raFile.readByte();
                                            }
                                        }
                                    }
                                    else if ((realDataBytes % 8) != 0) {
                                        padBytes = 8 - (realDataBytes % 8);
                                        for (i = 0; i < padBytes; i++) {
                                            raFile.readByte();
                                        }
                                    }  
                                    try {
                                        image2.importData(nonLogicalField * intBuffer.length, intBuffer, true);
                                    }
                                    catch(IOException e) {
                                       MipavUtil.displayError("IOException on image2.importData(nonLogicalField * intBuffer.length, intBuffer, true)");
                                       throw e;
                                    }
                                }
                                else {
                                    if (nonLogicalField == 0) {
                                        image2 = new ModelImage(ModelStorageBase.DCOMPLEX, imageExtents, fileName); 
                                        fileInfo2.setDataType(ModelStorageBase.DCOMPLEX);
                                    }
                                    buffer = new byte[realDataBytes];
                                    raFile.read(buffer);
                                    intNumber = realDataBytes/4;
                                    realDBuffer = new double[intNumber];
                                    numberSlices = intNumber/sliceSize;
                                    j = 0;
                                    for (s = 0; s < numberSlices; s++) {
                                        for (x = 0; x < imageExtents[1]; x++) {
                                            for (y = 0; y < imageExtents[0]; y++) {
                                                index = 4*(x + imageExtents[1] * y + s * sliceSize);
                                                b1 = buffer[index] & 0xff;
                                                b2 = buffer[index+1] & 0xff;
                                                b3 = buffer[index+2] & 0xff;
                                                b4 = buffer[index+3] & 0xff;
                                                if (endianess == BIG_ENDIAN) {
                                                    realDBuffer[j++] = (double)((b1 << 24) | (b2 << 16) | (b3 << 8) | b4);
                                                }
                                                else {
                                                    realDBuffer[j++] = (double)((b4 << 24) | (b3 << 16) | (b2 << 8) | b1);
                                                }
                                            }
                                        }
                                    }
                                    
                                    if (haveSmallRealData) {
                                        if (realDataBytes < 4) {
                                            padBytes = 4 - realDataBytes;
                                            for (i = 0; i < padBytes; i++) {
                                                raFile.readByte();
                                            }
                                        }
                                    }
                                    else if ((realDataBytes % 8) != 0) {
                                        padBytes = 8 - (realDataBytes % 8);
                                        for (i = 0; i < padBytes; i++) {
                                            raFile.readByte();
                                        }
                                    }    
                                    imaginaryDataType = getInt(endianess);
                                    if ((imaginaryDataType & 0xffff0000) != 0) {
                                        // Small data element format    
                                        imaginaryDataBytes = (imaginaryDataType & 0xffff0000) >>> 16;
                                        imaginaryDataType = imaginaryDataType & 0xffff;
                                        haveSmallImaginaryData = true;
                                    }
                                    else {
                                        imaginaryDataBytes = getInt(endianess);
                                        haveSmallImaginaryData = false;
                                    }
                                    if (imaginaryDataType == miINT32) {
                                        Preferences.debug("imaginaryDataType == miINT32 as expected\n", Preferences.DEBUG_FILEIO);
                                    }
                                    else {
                                        Preferences.debug("imaginaryDataType unexpectedly == " + imaginaryDataType + "\n", 
                                                Preferences.DEBUG_FILEIO);
                                    }
                                    
                                    if (imaginaryDataBytes == realDataBytes) {
                                        Preferences.debug("imaginaryDataBytes == realDataBytes as expected\n", 
                                                Preferences.DEBUG_FILEIO);
                                    }
                                    else {
                                        Preferences.debug("imaginaryDataBytes unexpectedly != realDataBytes\n", 
                                                Preferences.DEBUG_FILEIO);
                                    }
                                    raFile.read(buffer);
                                    imaginaryDBuffer = new double[intNumber];
                                    j = 0;
                                    for (s = 0; s < numberSlices; s++) {
                                        for (x = 0; x < imageExtents[1]; x++) {
                                            for (y = 0; y < imageExtents[0]; y++) {
                                                index = 4*(x + imageExtents[1] * y + s * sliceSize);
                                                b1 = buffer[index] & 0xff;
                                                b2 = buffer[index+1] & 0xff;
                                                b3 = buffer[index+2] & 0xff;
                                                b4 = buffer[index+3] & 0xff;
                                                if (endianess == BIG_ENDIAN) {
                                                    imaginaryDBuffer[j++] = (double)((b1 << 24) | (b2 << 16) | (b3 << 8) | b4);
                                                }
                                                else {
                                                    imaginaryDBuffer[j++] = (double)((b4 << 24) | (b3 << 16) | (b2 << 8) | b1);
                                                }
                                            }
                                        }
                                    }
                                    
                                    if (haveSmallImaginaryData) {
                                        if (imaginaryDataBytes < 4) {
                                            padBytes = 4 - imaginaryDataBytes;
                                            for (i = 0; i < padBytes; i++) {
                                                raFile.readByte();
                                            }
                                        }
                                    }
                                    else if ((imaginaryDataBytes % 8) != 0) {
                                        padBytes = 8 - (imaginaryDataBytes % 8);
                                        for (i = 0; i < padBytes; i++) {
                                            raFile.readByte();
                                        }
                                    }   
                                    
                                    try {
                                        image2.importDComplexData(2 * nonLogicalField * realDBuffer.length, realDBuffer, imaginaryDBuffer, true, logMagDisplay);
                                    }
                                    catch(IOException e) {
                                       MipavUtil.displayError("IOException on image2.importDComplexData(2 * nonLogicalField * realDBuffer.length, " +
                                            "realDBuffer, imaginaryDBuffer, true, logMagDisplay)");
                                       throw e;
                                    }
                                }
                                break;
                            case miUINT32:
                                Preferences.debug("Real data type = miUINT32\n", Preferences.DEBUG_FILEIO);
                                Preferences.debug("Real data bytes = " + realDataBytes + "\n", Preferences.DEBUG_FILEIO);
                                if (!complexFlag) {
                                    if (nonLogicalField == 0) {
                                        image2 = new ModelImage(ModelStorageBase.UINTEGER, imageExtents, fileName);
                                        fileInfo2.setDataType(ModelStorageBase.UINTEGER);
                                    }
                                    buffer = new byte[realDataBytes];
                                    raFile.read(buffer);
                                    intNumber = realDataBytes/4;
                                    longBuffer = new long[intNumber];
                                    numberSlices = intNumber/sliceSize;
                                    j = 0;
                                    for (s = 0; s < numberSlices; s++) {
                                        for (x = 0; x < imageExtents[1]; x++) {
                                            for (y = 0; y < imageExtents[0]; y++) {
                                                index = 4*(x + imageExtents[1] * y + s * sliceSize);
                                                b1L = buffer[index] & 0xffL;
                                                b2L = buffer[index+1] & 0xffL;
                                                b3L = buffer[index+2] & 0xffL;
                                                b4L = buffer[index+3] & 0xffL;
                                                if (endianess == BIG_ENDIAN) {
                                                    longBuffer[j++] = ((b1L << 24) | (b2L << 16) | (b3L << 8) | b4L);
                                                }
                                                else {
                                                    longBuffer[j++] = ((b4L << 24) | (b3L << 16) | (b2L << 8) | b1L);
                                                }
                                            }
                                        }
                                    }
                                    
                                    if (haveSmallRealData) {
                                        if (realDataBytes < 4) {
                                            padBytes = 4 - realDataBytes;
                                            for (i = 0; i < padBytes; i++) {
                                                raFile.readByte();
                                            }
                                        }
                                    }
                                    else if ((realDataBytes % 8) != 0) {
                                        padBytes = 8 - (realDataBytes % 8);
                                        for (i = 0; i < padBytes; i++) {
                                            raFile.readByte();
                                        }
                                    }  
                                    try {
                                        image2.importUData(nonLogicalField * longBuffer.length, longBuffer, true);
                                    }
                                    catch(IOException e) {
                                       MipavUtil.displayError("IOException on image2.importUData(nonLogicalField * longBuffer.length, longBuffer, true)");
                                       throw e;
                                    }
                                }
                                else {
                                    if (nonLogicalField == 0) {
                                        image2 = new ModelImage(ModelStorageBase.DCOMPLEX, imageExtents, fileName);
                                        fileInfo2.setDataType(ModelStorageBase.DCOMPLEX);
                                    }
                                    buffer = new byte[realDataBytes];
                                    raFile.read(buffer);
                                    intNumber = realDataBytes/4;
                                    realDBuffer = new double[intNumber];
                                    numberSlices = intNumber/sliceSize;
                                    j = 0;
                                    for (s = 0; s < numberSlices; s++) {
                                        for (x = 0; x < imageExtents[1]; x++) {
                                            for (y = 0; y < imageExtents[0]; y++) {
                                                index = 4*(x + imageExtents[1] * y + s * sliceSize);
                                                b1L = buffer[index] & 0xffL;
                                                b2L = buffer[index+1] & 0xffL;
                                                b3L = buffer[index+2] & 0xffL;
                                                b4L = buffer[index+3] & 0xffL;
                                                if (endianess == BIG_ENDIAN) {
                                                    realDBuffer[j++] = (double)((b1L << 24) | (b2L << 16) | (b3L << 8) | b4L);
                                                }
                                                else {
                                                    realDBuffer[j++] = (double)((b4L << 24) | (b3L << 16) | (b2L << 8) | b1L);
                                                }
                                            }
                                        }
                                    }
                                    
                                    if (haveSmallRealData) {
                                        if (realDataBytes < 4) {
                                            padBytes = 4 - realDataBytes;
                                            for (i = 0; i < padBytes; i++) {
                                                raFile.readByte();
                                            }
                                        }
                                    }
                                    else if ((realDataBytes % 8) != 0) {
                                        padBytes = 8 - (realDataBytes % 8);
                                        for (i = 0; i < padBytes; i++) {
                                            raFile.readByte();
                                        }
                                    }   
                                    imaginaryDataType = getInt(endianess);
                                    if ((imaginaryDataType & 0xffff0000) != 0) {
                                        // Small data element format    
                                        imaginaryDataBytes = (imaginaryDataType & 0xffff0000) >>> 16;
                                        imaginaryDataType = imaginaryDataType & 0xffff;
                                        haveSmallImaginaryData = true;
                                    }
                                    else {
                                        imaginaryDataBytes = getInt(endianess);
                                        haveSmallImaginaryData = false;
                                    }
                                    if (imaginaryDataType == miUINT32) {
                                        Preferences.debug("imaginaryDataType == miUINT32 as expected\n", Preferences.DEBUG_FILEIO);
                                    }
                                    else {
                                        Preferences.debug("imaginaryDataType unexpectedly == " + imaginaryDataType + "\n", 
                                                Preferences.DEBUG_FILEIO);
                                    }
                                    
                                    if (imaginaryDataBytes == realDataBytes) {
                                        Preferences.debug("imaginaryDataBytes == realDataBytes as expected\n", 
                                                Preferences.DEBUG_FILEIO);
                                    }
                                    else {
                                        Preferences.debug("imaginaryDataBytes unexpectedly != realDataBytes\n", 
                                                Preferences.DEBUG_FILEIO);
                                    }
                                    raFile.read(buffer);
                                    imaginaryDBuffer = new double[intNumber];
                                    j = 0;
                                    for (s = 0; s < numberSlices; s++) {
                                        for (x = 0; x < imageExtents[1]; x++) {
                                            for (y = 0; y < imageExtents[0]; y++) {
                                                index = 4*(x + imageExtents[1] * y + s * sliceSize);
                                                b1L = buffer[index] & 0xffL;
                                                b2L = buffer[index+1] & 0xffL;
                                                b3L = buffer[index+2] & 0xffL;
                                                b4L = buffer[index+3] & 0xffL;
                                                if (endianess == BIG_ENDIAN) {
                                                    imaginaryDBuffer[j++] = (double)((b1L << 24) | (b2L << 16) | (b3L << 8) | b4L);
                                                }
                                                else {
                                                    imaginaryDBuffer[j++] = (double)((b4L << 24) | (b3L << 16) | (b2L << 8) | b1L);
                                                }
                                            }
                                        }
                                    }
                                    
                                    if (haveSmallImaginaryData) {
                                        if (imaginaryDataBytes < 4) {
                                            padBytes = 4 - imaginaryDataBytes;
                                            for (i = 0; i < padBytes; i++) {
                                                raFile.readByte();
                                            }
                                        }
                                    }
                                    else if ((imaginaryDataBytes % 8) != 0) {
                                        padBytes = 8 - (imaginaryDataBytes % 8);
                                        for (i = 0; i < padBytes; i++) {
                                            raFile.readByte();
                                        }
                                    }   
                                    
                                    try {
                                        image2.importDComplexData(2 * nonLogicalField * realDBuffer.length, realDBuffer, imaginaryDBuffer, true, logMagDisplay);
                                    }
                                    catch(IOException e) {
                                       MipavUtil.displayError("IOException on image2.importDComplexData(2 * nonLogicalField * realDBuffer.length," +
                                            " realDBuffer, imaginaryDBuffer, true, logMagDisplay)");
                                       throw e;
                                    }
                                }
                                break;
                            case miSINGLE:
                                Preferences.debug("Real data type = miSINGLE\n", Preferences.DEBUG_FILEIO);
                                Preferences.debug("Real data bytes = " + realDataBytes + "\n", Preferences.DEBUG_FILEIO);
                                if (!complexFlag) {
                                    if (nonLogicalField == 0) {
                                        image2 = new ModelImage(ModelStorageBase.FLOAT, imageExtents, fileName); 
                                        fileInfo2.setDataType(ModelStorageBase.FLOAT);
                                    }
                                    buffer = new byte[realDataBytes];
                                    raFile.read(buffer);
                                    floatNumber = realDataBytes/4;
                                    floatBuffer = new float[floatNumber];
                                    numberSlices = floatNumber/sliceSize;
                                    j = 0;
                                    for (s = 0; s < numberSlices; s++) {
                                        for (x = 0; x < imageExtents[1]; x++) {
                                            for (y = 0; y < imageExtents[0]; y++) {
                                                index = 4*(x + imageExtents[1] * y + s * sliceSize);
                                                b1 = buffer[index] & 0xff;
                                                b2 = buffer[index+1] & 0xff;
                                                b3 = buffer[index+2] & 0xff;
                                                b4 = buffer[index+3] & 0xff;
                                                if (endianess == BIG_ENDIAN) {
                                                    tmpInt = ((b1 << 24) | (b2 << 16) | (b3 << 8) | b4);
                                                }
                                                else {
                                                    tmpInt = ((b4 << 24) | (b3 << 16) | (b2 << 8) | b1);
                                                }
                                                floatBuffer[j++] = Float.intBitsToFloat(tmpInt);
                                            }
                                        }
                                    }
                                    
                                    if (haveSmallRealData) {
                                        if (realDataBytes < 4) {
                                            padBytes = 4 - realDataBytes;
                                            for (i = 0; i < padBytes; i++) {
                                                raFile.readByte();
                                            }
                                        }
                                    }
                                    else if ((realDataBytes % 8) != 0) {
                                        padBytes = 8 - (realDataBytes % 8);
                                        for (i = 0; i < padBytes; i++) {
                                            raFile.readByte();
                                        }
                                    }  
                                    try {
                                        image2.importData(nonLogicalField * floatBuffer.length, floatBuffer, true);
                                    }
                                    catch(IOException e) {
                                       MipavUtil.displayError("IOException on image2.importData(nonLogicalField * floatBuffer.length, floatBuffer, true)");
                                       throw e;
                                    }
                                }
                                else {
                                    if (nonLogicalField == 0) {
                                        image2 = new ModelImage(ModelStorageBase.COMPLEX, imageExtents, fileName);
                                        fileInfo2.setDataType(ModelStorageBase.COMPLEX);
                                    }
                                    buffer = new byte[realDataBytes];
                                    raFile.read(buffer);
                                    floatNumber = realDataBytes/4;
                                    realBuffer = new float[floatNumber];
                                    numberSlices = floatNumber/sliceSize;
                                    j = 0;
                                    for (s = 0; s < numberSlices; s++) {
                                        for (x = 0; x < imageExtents[1]; x++) {
                                            for (y = 0; y < imageExtents[0]; y++) {
                                                index = 4*(x + imageExtents[1] * y + s * sliceSize);
                                                b1 = buffer[index] & 0xff;
                                                b2 = buffer[index+1] & 0xff;
                                                b3 = buffer[index+2] & 0xff;
                                                b4 = buffer[index+3] & 0xff;
                                                if (endianess == BIG_ENDIAN) {
                                                    tmpInt = ((b1 << 24) | (b2 << 16) | (b3 << 8) | b4);
                                                }
                                                else {
                                                    tmpInt = ((b4 << 24) | (b3 << 16) | (b2 << 8) | b1);
                                                }
                                                realBuffer[j++] = Float.intBitsToFloat(tmpInt);
                                            }
                                        }
                                    }
                                    
                                    if (haveSmallRealData) {
                                        if (realDataBytes < 4) {
                                            padBytes = 4 - realDataBytes;
                                            for (i = 0; i < padBytes; i++) {
                                                raFile.readByte();
                                            }
                                        }
                                    }
                                    else if ((realDataBytes % 8) != 0) {
                                        padBytes = 8 - (realDataBytes % 8);
                                        for (i = 0; i < padBytes; i++) {
                                            raFile.readByte();
                                        }
                                    }  
                                    imaginaryDataType = getInt(endianess);
                                    if ((imaginaryDataType & 0xffff0000) != 0) {
                                        // Small data element format    
                                        imaginaryDataBytes = (imaginaryDataType & 0xffff0000) >>> 16;
                                        imaginaryDataType = imaginaryDataType & 0xffff;
                                        haveSmallImaginaryData = true;
                                    }
                                    else {
                                        imaginaryDataBytes = getInt(endianess);
                                        haveSmallImaginaryData = false;
                                    }
                                    if (imaginaryDataType == miSINGLE) {
                                        Preferences.debug("imaginaryDataType == miSINGLE as expected\n", Preferences.DEBUG_FILEIO);
                                    }
                                    else {
                                        Preferences.debug("imaginaryDataType unexpectedly == " + imaginaryDataType + "\n", 
                                                Preferences.DEBUG_FILEIO);
                                    }
                                    
                                    if (imaginaryDataBytes == realDataBytes) {
                                        Preferences.debug("imaginaryDataBytes == realDataBytes as expected\n", 
                                                Preferences.DEBUG_FILEIO);
                                    }
                                    else {
                                        Preferences.debug("imaginaryDataBytes unexpectedly != realDataBytes\n", 
                                                Preferences.DEBUG_FILEIO);
                                    }
                                    raFile.read(buffer);
                                    imaginaryBuffer = new float[floatNumber];
                                    j = 0;
                                    for (s = 0; s < numberSlices; s++) {
                                        for (x = 0; x < imageExtents[1]; x++) {
                                            for (y = 0; y < imageExtents[0]; y++) {
                                                index = 4*(x + imageExtents[1] * y + s * sliceSize);
                                                b1 = buffer[index] & 0xff;
                                                b2 = buffer[index+1] & 0xff;
                                                b3 = buffer[index+2] & 0xff;
                                                b4 = buffer[index+3] & 0xff;
                                                if (endianess == BIG_ENDIAN) {
                                                    tmpInt = ((b1 << 24) | (b2 << 16) | (b3 << 8) | b4);
                                                }
                                                else {
                                                    tmpInt = ((b4 << 24) | (b3 << 16) | (b2 << 8) | b1);
                                                }
                                                imaginaryBuffer[j++] = Float.intBitsToFloat(tmpInt);
                                            }
                                        }
                                    }
                                    
                                    if (haveSmallImaginaryData) {
                                        if (imaginaryDataBytes < 4) {
                                            padBytes = 4 - imaginaryDataBytes;
                                            for (i = 0; i < padBytes; i++) {
                                                raFile.readByte();
                                            }
                                        }
                                    }
                                    else if ((imaginaryDataBytes % 8) != 0) {
                                        padBytes = 8 - (imaginaryDataBytes % 8);
                                        for (i = 0; i < padBytes; i++) {
                                            raFile.readByte();
                                        }
                                    }   
                                    
                                    try {
                                        image2.importComplexData(2 * nonLogicalField * realBuffer.length, realBuffer, imaginaryBuffer, true, logMagDisplay);
                                    }
                                    catch(IOException e) {
                                       MipavUtil.displayError("IOException on image2.importComplexData(2 * nonLogicalField * realBuffer.length, " +
                                            "realBuffer, imaginaryBuffer, true, logMagDisplay)");
                                       throw e;
                                    }
                                }
                                break;
                            case miDOUBLE:
                                Preferences.debug("Real data type = miDOUBLE\n", Preferences.DEBUG_FILEIO);
                                Preferences.debug("Real data bytes = " + realDataBytes + "\n", Preferences.DEBUG_FILEIO);
                                if (isColor) {
                                    if (nonLogicalField == 0) {
                                        image2 = new ModelImage(ModelStorageBase.ARGB_FLOAT, imageExtents, fileName);
                                        fileInfo2.setDataType(ModelStorageBase.ARGB_FLOAT);
                                    }
                                    if ((realDataBytes % 3) == 0) {
                                        buffer = new byte[realDataBytes/3];
                                        doubleNumber = realDataBytes/24;
                                        floatBuffer = new float[doubleNumber];
                                        numberSlices = doubleNumber/sliceSize;
                                        for (i = 0; i < 3; i++) {
                                            raFile.read(buffer);
                                            j = 0;
                                            for (s = 0; s < numberSlices; s++) {
                                                for (x = 0; x < imageExtents[1]; x++) {
                                                    for (y = 0; y < imageExtents[0]; y++) { 
                                                        index = 8*(x + imageExtents[1] * y + s * sliceSize);
                                                        b1L = buffer[index] & 0xffL;
                                                        b2L = buffer[index+1] & 0xffL;
                                                        b3L = buffer[index+2] & 0xffL;
                                                        b4L = buffer[index+3] & 0xffL;
                                                        b5L = buffer[index+4] & 0xffL;
                                                        b6L = buffer[index+5] & 0xffL;
                                                        b7L = buffer[index+6] & 0xffL;
                                                        b8L = buffer[index+7] & 0xffL;
                                                        if (endianess == BIG_ENDIAN) {
                                                            tmpLong = ((b1L << 56) | (b2L << 48) | (b3L << 40) | (b4L << 32) |
                                                                       (b5L << 24) | (b6L << 16) | (b7L << 8) | b8L);   
                                                        }
                                                        else {
                                                            tmpLong = ((b8L << 56) | (b7L << 48) | (b6L << 40) | (b5L << 32) |
                                                                       (b4L << 24) | (b3L << 16) | (b2L << 8) | b1L);
                                                        }
                                                        floatBuffer[j++] = (float)(255.0 * Double.longBitsToDouble(tmpLong));
                                                    }
                                                }
                                            }
                                            
                                            try {
                                                image2.importRGBData(i+1, nonLogicalField * floatBuffer.length, floatBuffer, true);
                                            }
                                            catch(IOException e) {
                                               MipavUtil.displayError("IOException on image2.importRGBData(i," +
                                                    " nonLogicalField * floatBuffer.length, floatBuffer, true)");
                                               throw e;
                                            }
                                        } // if (i = 0; i < 3; i++)
            
                                        
                                        if (haveSmallRealData) {
                                            if (realDataBytes < 4) {
                                                padBytes = 4 - realDataBytes;
                                                for (i = 0; i < padBytes; i++) {
                                                    raFile.readByte();
                                                }
                                            }
                                        }
                                        else if ((realDataBytes % 8) != 0) {
                                            padBytes = 8 - (realDataBytes % 8);
                                            for (i = 0; i < padBytes; i++) {
                                                raFile.readByte();
                                            }
                                        } 
                                    
                                    } // if (realDataBytes % 3) == 0)
                                } // else if (isColor)
                                else if (!complexFlag) {
                                    if (nonLogicalField == 0) {
                                        image2 = new ModelImage(ModelStorageBase.DOUBLE, imageExtents, fileName); 
                                        fileInfo2.setDataType(ModelStorageBase.DOUBLE);
                                    }
                                    buffer = new byte[realDataBytes];
                                    raFile.read(buffer);
                                    doubleNumber = realDataBytes/8;
                                    doubleBuffer = new double[doubleNumber];
                                    numberSlices = doubleNumber/sliceSize;
                                    j = 0;
                                    for (s = 0; s < numberSlices; s++) {
                                        for (x = 0; x < imageExtents[1]; x++) {
                                            for (y = 0; y < imageExtents[0]; y++) {
                                                index = 8*(x + imageExtents[1] * y + s * sliceSize);
                                                b1L = buffer[index] & 0xffL;
                                                b2L = buffer[index+1] & 0xffL;
                                                b3L = buffer[index+2] & 0xffL;
                                                b4L = buffer[index+3] & 0xffL;
                                                b5L = buffer[index+4] & 0xffL;
                                                b6L = buffer[index+5] & 0xffL;
                                                b7L = buffer[index+6] & 0xffL;
                                                b8L = buffer[index+7] & 0xffL;
                                                if (endianess == BIG_ENDIAN) {
                                                    tmpLong = ((b1L << 56) | (b2L << 48) | (b3L << 40) | (b4L << 32) |
                                                               (b5L << 24) | (b6L << 16) | (b7L << 8) | b8L);   
                                                }
                                                else {
                                                    tmpLong = ((b8L << 56) | (b7L << 48) | (b6L << 40) | (b5L << 32) |
                                                               (b4L << 24) | (b3L << 16) | (b2L << 8) | b1L);
                                                }
                                                doubleBuffer[j++] = Double.longBitsToDouble(tmpLong);
                                            }
                                        }
                                    }
                                    
                                    if (haveSmallRealData) {
                                        if (realDataBytes < 4) {
                                            padBytes = 4 - realDataBytes;
                                            for (i = 0; i < padBytes; i++) {
                                                raFile.readByte();
                                            }
                                        }
                                    }
                                    else if ((realDataBytes % 8) != 0) {
                                        padBytes = 8 - (realDataBytes % 8);
                                        for (i = 0; i < padBytes; i++) {
                                            raFile.readByte();
                                        }
                                    }  
                                    try {
                                        image2.importData(nonLogicalField * doubleBuffer.length, doubleBuffer, true);
                                    }
                                    catch(IOException e) {
                                       MipavUtil.displayError("IOException on image2.importData(nonLogicalField * doubleBuffer.length, doubleBuffer, true)");
                                       throw e;
                                    }
                                }
                                else {
                                    if (nonLogicalField == 0) {
                                        image2 = new ModelImage(ModelStorageBase.DCOMPLEX, imageExtents, fileName); 
                                        fileInfo2.setDataType(ModelStorageBase.DCOMPLEX);
                                    }
                                    buffer = new byte[realDataBytes];
                                    raFile.read(buffer);
                                    doubleNumber = realDataBytes/8;
                                    realDBuffer = new double[doubleNumber];
                                    numberSlices = doubleNumber/sliceSize;
                                    j = 0;
                                    for (s = 0; s < numberSlices; s++) {
                                        for (x = 0; x < imageExtents[1]; x++) {
                                            for (y = 0; y < imageExtents[0]; y++) {
                                                index = 8*(x + imageExtents[1] * y + s * sliceSize);
                                                b1L = buffer[index] & 0xffL;
                                                b2L = buffer[index+1] & 0xffL;
                                                b3L = buffer[index+2] & 0xffL;
                                                b4L = buffer[index+3] & 0xffL;
                                                b5L = buffer[index+4] & 0xffL;
                                                b6L = buffer[index+5] & 0xffL;
                                                b7L = buffer[index+6] & 0xffL;
                                                b8L = buffer[index+7] & 0xffL;
                                                if (endianess == BIG_ENDIAN) {
                                                    tmpLong = ((b1L << 56) | (b2L << 48) | (b3L << 40) | (b4L << 32) |
                                                               (b5L << 24) | (b6L << 16) | (b7L << 8) | b8L);   
                                                }
                                                else {
                                                    tmpLong = ((b8L << 56) | (b7L << 48) | (b6L << 40) | (b5L << 32) |
                                                               (b4L << 24) | (b3L << 16) | (b2L << 8) | b1L);
                                                }
                                                realDBuffer[j++] = Double.longBitsToDouble(tmpLong);
                                            }
                                        }
                                    }
                                    
                                    if (haveSmallRealData) {
                                        if (realDataBytes < 4) {
                                            padBytes = 4 - realDataBytes;
                                            for (i = 0; i < padBytes; i++) {
                                                raFile.readByte();
                                            }
                                        }
                                    }
                                    else if ((realDataBytes % 8) != 0) {
                                        padBytes = 8 - (realDataBytes % 8);
                                        for (i = 0; i < padBytes; i++) {
                                            raFile.readByte();
                                        }
                                    }    
                                    imaginaryDataType = getInt(endianess);
                                    if ((imaginaryDataType & 0xffff0000) != 0) {
                                        // Small data element format    
                                        imaginaryDataBytes = (imaginaryDataType & 0xffff0000) >>> 16;
                                        imaginaryDataType = imaginaryDataType & 0xffff;
                                        haveSmallImaginaryData = true;
                                    }
                                    else {
                                        imaginaryDataBytes = getInt(endianess);
                                        haveSmallImaginaryData = false;
                                    }
                                    if (imaginaryDataType == miDOUBLE) {
                                        Preferences.debug("imaginaryDataType == miDOUBLE as expected\n", Preferences.DEBUG_FILEIO);
                                    }
                                    else {
                                        Preferences.debug("imaginaryDataType unexpectedly == " + imaginaryDataType + "\n", 
                                                Preferences.DEBUG_FILEIO);
                                    }
                                    
                                    if (imaginaryDataBytes == realDataBytes) {
                                        Preferences.debug("imaginaryDataBytes == realDataBytes as expected\n", 
                                                Preferences.DEBUG_FILEIO);
                                    }
                                    else {
                                        Preferences.debug("imaginaryDataBytes unexpectedly != realDataBytes\n", 
                                                Preferences.DEBUG_FILEIO);
                                    }
                                    raFile.read(buffer);
                                    imaginaryDBuffer = new double[doubleNumber];
                                    j = 0;
                                    for (s = 0; s < numberSlices; s++) {
                                        for (x = 0; x < imageExtents[1]; x++) {
                                            for (y = 0; y < imageExtents[0]; y++) {
                                                index = 8*(x + imageExtents[1] * y + s * sliceSize);
                                                b1L = buffer[index] & 0xffL;
                                                b2L = buffer[index+1] & 0xffL;
                                                b3L = buffer[index+2] & 0xffL;
                                                b4L = buffer[index+3] & 0xffL;
                                                b5L = buffer[index+4] & 0xffL;
                                                b6L = buffer[index+5] & 0xffL;
                                                b7L = buffer[index+6] & 0xffL;
                                                b8L = buffer[index+7] & 0xffL;
                                                if (endianess == BIG_ENDIAN) {
                                                    tmpLong = ((b1L << 56) | (b2L << 48) | (b3L << 40) | (b4L << 32) |
                                                               (b5L << 24) | (b6L << 16) | (b7L << 8) | b8L);   
                                                }
                                                else {
                                                    tmpLong = ((b8L << 56) | (b7L << 48) | (b6L << 40) | (b5L << 32) |
                                                               (b4L << 24) | (b3L << 16) | (b2L << 8) | b1L);
                                                }
                                                imaginaryDBuffer[j++] = Double.longBitsToDouble(tmpLong);
                                            }
                                        }
                                    }
                                    
                                    if (haveSmallImaginaryData) {
                                        if (imaginaryDataBytes < 4) {
                                            padBytes = 4 - imaginaryDataBytes;
                                            for (i = 0; i < padBytes; i++) {
                                                raFile.readByte();
                                            }
                                        }
                                    }
                                    else if ((imaginaryDataBytes % 8) != 0) {
                                        padBytes = 8 - (imaginaryDataBytes % 8);
                                        for (i = 0; i < padBytes; i++) {
                                            raFile.readByte();
                                        }
                                    }   
                                    
                                    try {
                                        image2.importDComplexData(2 * nonLogicalField * realDBuffer.length, realDBuffer, imaginaryDBuffer, true, logMagDisplay);
                                    }
                                    catch(IOException e) {
                                       MipavUtil.displayError("IOException on image2.importDComplexData(2 * nonLogicalField * realDBuffer.length, " +
                                            "realDBuffer, imaginaryDBuffer, true, logMagDisplay)");
                                       throw e;
                                    }
                                }
                                break;
                            case miINT64:
                                Preferences.debug("Real data type = miINT64\n", Preferences.DEBUG_FILEIO);
                                Preferences.debug("Real data bytes = " + realDataBytes + "\n", Preferences.DEBUG_FILEIO);
                                if (!complexFlag) {
                                    if (nonLogicalField == 0) {
                                        image2 = new ModelImage(ModelStorageBase.LONG, imageExtents, fileName);
                                        fileInfo2.setDataType(ModelStorageBase.LONG);
                                    }
                                    buffer = new byte[realDataBytes];
                                    raFile.read(buffer);
                                    longNumber = realDataBytes/8;
                                    longBuffer = new long[longNumber];
                                    numberSlices = longNumber/sliceSize;
                                    j = 0;
                                    for (s = 0; s < numberSlices; s++) {
                                        for (x = 0; x < imageExtents[1]; x++) {
                                            for (y = 0; y < imageExtents[0]; y++) {
                                                index = 8*(x + imageExtents[1] * y + s * sliceSize);
                                                b1L = buffer[index] & 0xffL;
                                                b2L = buffer[index+1] & 0xffL;
                                                b3L = buffer[index+2] & 0xffL;
                                                b4L = buffer[index+3] & 0xffL;
                                                b5L = buffer[index+4] & 0xffL;
                                                b6L = buffer[index+5] & 0xffL;
                                                b7L = buffer[index+6] & 0xffL;
                                                b8L = buffer[index+7] & 0xffL;
                                                if (endianess == BIG_ENDIAN) {
                                                    longBuffer[j++] = ((b1L << 56) | (b2L << 48) | (b3L << 40) | (b4L << 32) |
                                                               (b5L << 24) | (b6L << 16) | (b7L << 8) | b8L);   
                                                }
                                                else {
                                                    longBuffer[j++] = ((b8L << 56) | (b7L << 48) | (b6L << 40) | (b5L << 32) |
                                                               (b4L << 24) | (b3L << 16) | (b2L << 8) | b1L);
                                                }
                                            }
                                        }
                                    }
                                    
                                    if (haveSmallRealData) {
                                        if (realDataBytes < 4) {
                                            padBytes = 4 - realDataBytes;
                                            for (i = 0; i < padBytes; i++) {
                                                raFile.readByte();
                                            }
                                        }
                                    }
                                    else if ((realDataBytes % 8) != 0) {
                                        padBytes = 8 - (realDataBytes % 8);
                                        for (i = 0; i < padBytes; i++) {
                                            raFile.readByte();
                                        }
                                    }  
                                    try {
                                        image2.importData(nonLogicalField * longBuffer.length, longBuffer, true);
                                    }
                                    catch(IOException e) {
                                       MipavUtil.displayError("IOException on image2.importData(nonLogicalField * longBuffer.length, longBuffer, true)");
                                       throw e;
                                    }
                                }
                                else {
                                    if (nonLogicalField == 0) {
                                        image2 = new ModelImage(ModelStorageBase.DCOMPLEX, imageExtents, fileName); 
                                        fileInfo2.setDataType(ModelStorageBase.DCOMPLEX);
                                    }
                                    buffer = new byte[realDataBytes];
                                    raFile.read(buffer);
                                    longNumber = realDataBytes/8;
                                    realDBuffer = new double[longNumber];
                                    numberSlices = longNumber/sliceSize;
                                    j = 0;
                                    for (s = 0; s < numberSlices; s++) {
                                        for (x = 0; x < imageExtents[1]; x++) {
                                            for (y = 0; y < imageExtents[0]; y++) {
                                                index = 8*(x + imageExtents[1] * y + s * sliceSize);
                                                b1L = buffer[index] & 0xffL;
                                                b2L = buffer[index+1] & 0xffL;
                                                b3L = buffer[index+2] & 0xffL;
                                                b4L = buffer[index+3] & 0xffL;
                                                b5L = buffer[index+4] & 0xffL;
                                                b6L = buffer[index+5] & 0xffL;
                                                b7L = buffer[index+6] & 0xffL;
                                                b8L = buffer[index+7] & 0xffL;
                                                if (endianess == BIG_ENDIAN) {
                                                    realDBuffer[j++] = (double)((b1L << 56) | (b2L << 48) | (b3L << 40) | (b4L << 32) |
                                                               (b5L << 24) | (b6L << 16) | (b7L << 8) | b8L);   
                                                }
                                                else {
                                                    realDBuffer[j++] = (double)((b8L << 56) | (b7L << 48) | (b6L << 40) | (b5L << 32) |
                                                               (b4L << 24) | (b3L << 16) | (b2L << 8) | b1L);
                                                }
                                            }
                                        }
                                    }
                                    
                                    if (haveSmallRealData) {
                                        if (realDataBytes < 4) {
                                            padBytes = 4 - realDataBytes;
                                            for (i = 0; i < padBytes; i++) {
                                                raFile.readByte();
                                            }
                                        }
                                    }
                                    else if ((realDataBytes % 8) != 0) {
                                        padBytes = 8 - (realDataBytes % 8);
                                        for (i = 0; i < padBytes; i++) {
                                            raFile.readByte();
                                        }
                                    }   
                                    imaginaryDataType = getInt(endianess);
                                    if ((imaginaryDataType & 0xffff0000) != 0) {
                                        // Small data element format    
                                        imaginaryDataBytes = (imaginaryDataType & 0xffff0000) >>> 16;
                                        imaginaryDataType = imaginaryDataType & 0xffff;
                                        haveSmallImaginaryData = true;
                                    }
                                    else {
                                        imaginaryDataBytes = getInt(endianess);
                                        haveSmallImaginaryData = false;
                                    }
                                    if (imaginaryDataType == miINT64) {
                                        Preferences.debug("imaginaryDataType == miINT64 as expected\n", Preferences.DEBUG_FILEIO);
                                    }
                                    else {
                                        Preferences.debug("imaginaryDataType unexpectedly == " + imaginaryDataType + "\n", 
                                                Preferences.DEBUG_FILEIO);
                                    }
                                    
                                    if (imaginaryDataBytes == realDataBytes) {
                                        Preferences.debug("imaginaryDataBytes == realDataBytes as expected\n", 
                                                Preferences.DEBUG_FILEIO);
                                    }
                                    else {
                                        Preferences.debug("imaginaryDataBytes unexpectedly != realDataBytes\n", 
                                                Preferences.DEBUG_FILEIO);
                                    }
                                    raFile.read(buffer);
                                    imaginaryDBuffer = new double[longNumber];
                                    j = 0;
                                    for (s = 0; s < numberSlices; s++) {
                                        for (x = 0; x < imageExtents[1]; x++) {
                                            for (y = 0; y < imageExtents[0]; y++) {
                                                index = 8*(x + imageExtents[1] * y + s * sliceSize);
                                                b1L = buffer[index] & 0xffL;
                                                b2L = buffer[index+1] & 0xffL;
                                                b3L = buffer[index+2] & 0xffL;
                                                b4L = buffer[index+3] & 0xffL;
                                                b5L = buffer[index+4] & 0xffL;
                                                b6L = buffer[index+5] & 0xffL;
                                                b7L = buffer[index+6] & 0xffL;
                                                b8L = buffer[index+7] & 0xffL;
                                                if (endianess == BIG_ENDIAN) {
                                                    imaginaryDBuffer[j++] = (double)((b1L << 56) | (b2L << 48) | (b3L << 40) | (b4L << 32) |
                                                               (b5L << 24) | (b6L << 16) | (b7L << 8) | b8L);   
                                                }
                                                else {
                                                    imaginaryDBuffer[j++] = (double)((b8L << 56) | (b7L << 48) | (b6L << 40) | (b5L << 32) |
                                                               (b4L << 24) | (b3L << 16) | (b2L << 8) | b1L);
                                                }
                                            }
                                        }
                                    }
                                    
                                    if (haveSmallImaginaryData) {
                                        if (imaginaryDataBytes < 4) {
                                            padBytes = 4 - imaginaryDataBytes;
                                            for (i = 0; i < padBytes; i++) {
                                                raFile.readByte();
                                            }
                                        }
                                    }
                                    else if ((imaginaryDataBytes % 8) != 0) {
                                        padBytes = 8 - (imaginaryDataBytes % 8);
                                        for (i = 0; i < padBytes; i++) {
                                            raFile.readByte();
                                        }
                                    }   
                                    
                                    try {
                                        image2.importDComplexData(2 * nonLogicalField * realDBuffer.length, realDBuffer, imaginaryDBuffer, true, logMagDisplay);
                                    }
                                    catch(IOException e) {
                                       MipavUtil.displayError("IOException on image2.importDComplexData(2 * nonLogicalField * realDBuffer.length, " +
                                            "realDBuffer, imaginaryDBuffer, true, logMagDisplay)");
                                       throw e;
                                    }
                                }
                                break;
                            case miUINT64:
                                Preferences.debug("Real data type = miUINT64\n", Preferences.DEBUG_FILEIO);
                                Preferences.debug("Real data bytes = " + realDataBytes + "\n", Preferences.DEBUG_FILEIO);
                                if (!complexFlag) {
                                    if (nonLogicalField == 0) {
                                        image2 = new ModelImage(ModelStorageBase.LONG, imageExtents, fileName); 
                                        fileInfo2.setDataType(ModelStorageBase.LONG);
                                    }
                                    buffer = new byte[realDataBytes];
                                    raFile.read(buffer);
                                    longNumber = realDataBytes/8;
                                    longBuffer = new long[longNumber];
                                    numberSlices = longNumber/sliceSize;
                                    j = 0;
                                    for (s = 0; s < numberSlices; s++) {
                                        for (x = 0; x < imageExtents[1]; x++) {
                                            for (y = 0; y < imageExtents[0]; y++) {
                                                index = 8*(x + imageExtents[1] * y + s * sliceSize);
                                                b1L = buffer[index] & 0xffL;
                                                b2L = buffer[index+1] & 0xffL;
                                                b3L = buffer[index+2] & 0xffL;
                                                b4L = buffer[index+3] & 0xffL;
                                                b5L = buffer[index+4] & 0xffL;
                                                b6L = buffer[index+5] & 0xffL;
                                                b7L = buffer[index+6] & 0xffL;
                                                b8L = buffer[index+7] & 0xffL;
                                                if (endianess == BIG_ENDIAN) {
                                                    longBuffer[j++] = ((b1L << 56) | (b2L << 48) | (b3L << 40) | (b4L << 32) |
                                                               (b5L << 24) | (b6L << 16) | (b7L << 8) | b8L);   
                                                }
                                                else {
                                                    longBuffer[j++] = ((b8L << 56) | (b7L << 48) | (b6L << 40) | (b5L << 32) |
                                                               (b4L << 24) | (b3L << 16) | (b2L << 8) | b1L);
                                                }
                                            }
                                        }
                                    }
                                    
                                    if (haveSmallRealData) {
                                        if (realDataBytes < 4) {
                                            padBytes = 4 - realDataBytes;
                                            for (i = 0; i < padBytes; i++) {
                                                raFile.readByte();
                                            }
                                        }
                                    }
                                    else if ((realDataBytes % 8) != 0) {
                                        padBytes = 8 - (realDataBytes % 8);
                                        for (i = 0; i < padBytes; i++) {
                                            raFile.readByte();
                                        }
                                    }  
                                    try {
                                        image2.importData(nonLogicalField * longBuffer.length, longBuffer, true);
                                    }
                                    catch(IOException e) {
                                       MipavUtil.displayError("IOException on image2.importData(nonLogicalField * longBuffer.length, longBuffer, true)");
                                       throw e;
                                    }
                                }
                                else {
                                    if (nonLogicalField == 0) {
                                        image2 = new ModelImage(ModelStorageBase.DCOMPLEX, imageExtents, fileName); 
                                        fileInfo2.setDataType(ModelStorageBase.DCOMPLEX);
                                    }
                                    buffer = new byte[realDataBytes];
                                    raFile.read(buffer);
                                    longNumber = realDataBytes/8;
                                    realDBuffer = new double[longNumber];
                                    numberSlices = longNumber/sliceSize;
                                    j = 0;
                                    for (s = 0; s < numberSlices; s++) {
                                        for (x = 0; x < imageExtents[1]; x++) {
                                            for (y = 0; y < imageExtents[0]; y++) {
                                                index = 8*(x + imageExtents[1] * y + s * sliceSize);
                                                b1L = buffer[index] & 0xffL;
                                                b2L = buffer[index+1] & 0xffL;
                                                b3L = buffer[index+2] & 0xffL;
                                                b4L = buffer[index+3] & 0xffL;
                                                b5L = buffer[index+4] & 0xffL;
                                                b6L = buffer[index+5] & 0xffL;
                                                b7L = buffer[index+6] & 0xffL;
                                                b8L = buffer[index+7] & 0xffL;
                                                if (endianess == BIG_ENDIAN) {
                                                    realDBuffer[j++] = (double)((b1L << 56) | (b2L << 48) | (b3L << 40) | (b4L << 32) |
                                                               (b5L << 24) | (b6L << 16) | (b7L << 8) | b8L);   
                                                }
                                                else {
                                                    realDBuffer[j++] = (double)((b8L << 56) | (b7L << 48) | (b6L << 40) | (b5L << 32) |
                                                               (b4L << 24) | (b3L << 16) | (b2L << 8) | b1L);
                                                }
                                            }
                                        }
                                    }
                                    
                                    if (haveSmallRealData) {
                                        if (realDataBytes < 4) {
                                            padBytes = 4 - realDataBytes;
                                            for (i = 0; i < padBytes; i++) {
                                                raFile.readByte();
                                            }
                                        }
                                    }
                                    else if ((realDataBytes % 8) != 0) {
                                        padBytes = 8 - (realDataBytes % 8);
                                        for (i = 0; i < padBytes; i++) {
                                            raFile.readByte();
                                        }
                                    }  
                                    imaginaryDataType = getInt(endianess);
                                    if ((imaginaryDataType & 0xffff0000) != 0) {
                                        // Small data element format    
                                        imaginaryDataBytes = (imaginaryDataType & 0xffff0000) >>> 16;
                                        imaginaryDataType = imaginaryDataType & 0xffff;
                                        haveSmallImaginaryData = true;
                                    }
                                    else {
                                        imaginaryDataBytes = getInt(endianess);
                                        haveSmallImaginaryData = false;
                                    }
                                    if (imaginaryDataType == miUINT64) {
                                        Preferences.debug("imaginaryDataType == miUINT64 as expected\n", Preferences.DEBUG_FILEIO);
                                    }
                                    else {
                                        Preferences.debug("imaginaryDataType unexpectedly == " + imaginaryDataType + "\n", 
                                                Preferences.DEBUG_FILEIO);
                                    }
                                     
                                    if (imaginaryDataBytes == realDataBytes) {
                                        Preferences.debug("imaginaryDataBytes == realDataBytes as expected\n", 
                                                Preferences.DEBUG_FILEIO);
                                    }
                                    else {
                                        Preferences.debug("imaginaryDataBytes unexpectedly != realDataBytes\n", 
                                                Preferences.DEBUG_FILEIO);
                                    }
                                    raFile.read(buffer);
                                    imaginaryDBuffer = new double[longNumber];
                                    j = 0;
                                    for (s = 0; s < numberSlices; s++) {
                                        for (x = 0; x < imageExtents[1]; x++) {
                                            for (y = 0; y < imageExtents[0]; y++) {
                                                index = 8*(x + imageExtents[1] * y + s * sliceSize);
                                                b1L = buffer[index] & 0xffL;
                                                b2L = buffer[index+1] & 0xffL;
                                                b3L = buffer[index+2] & 0xffL;
                                                b4L = buffer[index+3] & 0xffL;
                                                b5L = buffer[index+4] & 0xffL;
                                                b6L = buffer[index+5] & 0xffL;
                                                b7L = buffer[index+6] & 0xffL;
                                                b8L = buffer[index+7] & 0xffL;
                                                if (endianess == BIG_ENDIAN) {
                                                    imaginaryDBuffer[j++] = (double)((b1L << 56) | (b2L << 48) | (b3L << 40) | (b4L << 32) |
                                                               (b5L << 24) | (b6L << 16) | (b7L << 8) | b8L);   
                                                }
                                                else {
                                                    imaginaryDBuffer[j++] = (double)((b8L << 56) | (b7L << 48) | (b6L << 40) | (b5L << 32) |
                                                               (b4L << 24) | (b3L << 16) | (b2L << 8) | b1L);
                                                }
                                            }
                                        }
                                    }
                                    
                                    if (haveSmallImaginaryData) {
                                        if (imaginaryDataBytes < 4) {
                                            padBytes = 4 - imaginaryDataBytes;
                                            for (i = 0; i < padBytes; i++) {
                                                raFile.readByte();
                                            }
                                        }
                                    }
                                    else if ((imaginaryDataBytes % 8) != 0) {
                                        padBytes = 8 - (imaginaryDataBytes % 8);
                                        for (i = 0; i < padBytes; i++) {
                                            raFile.readByte();
                                        }
                                    }   
                                    
                                    try {
                                        image2.importDComplexData(2 * nonLogicalField * realDBuffer.length, realDBuffer, imaginaryDBuffer, true, logMagDisplay);
                                    }
                                    catch(IOException e) {
                                       MipavUtil.displayError("IOException on image2.importDComplexData(2 * nonLogicalField * realDBuffer.length, " +
                                            "realDBuffer, imaginaryDBuffer, true, logMagDisplay)");
                                       throw e;
                                    }
                                }
                                break;
                            default:
                                Preferences.debug("Illegal data type = " + realDataType + "\n", Preferences.DEBUG_FILEIO);
                                Preferences.debug("Real data bytes = " + realDataBytes + "\n", Preferences.DEBUG_FILEIO);
                            }   
                            
                        } // else imagesFound > 1
                        } // for (field = 0; field < fieldNumber; field++)
                        if (logicalFields >= 1) {
                            adjustedFieldDim = imageExtents[imageExtents.length-1] - logicalFields;
                            if (adjustedFieldDim >= 2) {
                                newExtents = new int[imageExtents.length];
                                for (i = 0; i < imageExtents.length - 1; i++) {
                                    newExtents[i] = imageExtents[i];
                                }
                                newExtents[imageExtents.length-1] = adjustedFieldDim;
                            } // if (adjustedFieldDim >= 2)
                            else {
                                newExtents = new int[imageExtents.length-1];
                                for (i = 0; i < imageExtents.length - 1; i++) {
                                    newExtents[i] = imageExtents[i];
                                }
                            }
                            totalNumber = 1;
                            imageSlices = 1;
                            for (i = 0; i < newExtents.length; i++) {
                                totalNumber *= newExtents[i];
                                if (i > 1) {
                                    imageSlices *= newExtents[i];
                                }
                            }
                        } // if (logicalFields >= 1)
                        if (imagesFound == 1) {
                            if (logicalFields >= 1) {
                                switch(image.getType()) {
                                case ModelStorageBase.BYTE:
                                    buffer = new byte[totalNumber];
                                    try {
                                        image.exportData(0, totalNumber, buffer);
                                    }
                                    catch(IOException e) {
                                       MipavUtil.displayError("IOException on image.exportData(0, totalNumber, buffer)");
                                       throw e;
                                    }
                                    image.changeExtents(newExtents);
                                    image.recomputeDataSize();
                                    try {
                                        image.importData(0, buffer, true);
                                    }
                                    catch(IOException e) {
                                        MipavUtil.displayError("IOException on image.importData(0, buffer, true)");
                                        throw e;
                                    }
                                    break;
                                case ModelStorageBase.UBYTE:
                                    shortBuffer = new short[totalNumber];
                                    try {
                                        image.exportData(0, totalNumber, shortBuffer);
                                    }
                                    catch(IOException e) {
                                       MipavUtil.displayError("IOException on image.exportData(0, totalNumber, shortBuffer)");
                                       throw e;
                                    }
                                    image.changeExtents(newExtents);
                                    image.recomputeDataSize();
                                    try {
                                        image.importUData(0, shortBuffer, true);
                                    }
                                    catch(IOException e) {
                                        MipavUtil.displayError("IOException on image.importUData(0, shortBuffer, true)");
                                        throw e;
                                    }
                                    break;
                                case ModelStorageBase.SHORT:
                                    shortBuffer = new short[totalNumber];
                                    try {
                                        image.exportData(0, totalNumber, shortBuffer);
                                    }
                                    catch(IOException e) {
                                       MipavUtil.displayError("IOException on image.exportData(0, totalNumber, shortBuffer)");
                                       throw e;
                                    }
                                    image.changeExtents(newExtents);
                                    image.recomputeDataSize();
                                    try {
                                        image.importData(0, shortBuffer, true);
                                    }
                                    catch(IOException e) {
                                        MipavUtil.displayError("IOException on image.importData(0, shortBuffer, true)");
                                        throw e;
                                    }
                                    break;
                                case ModelStorageBase.USHORT:
                                    intBuffer = new int[totalNumber];
                                    try {
                                        image.exportData(0, totalNumber, intBuffer);
                                    }
                                    catch(IOException e) {
                                       MipavUtil.displayError("IOException on image.exportData(0, totalNumber, intBuffer)");
                                       throw e;
                                    }
                                    image.changeExtents(newExtents);
                                    image.recomputeDataSize();
                                    try {
                                        image.importUData(0, intBuffer, true);
                                    }
                                    catch(IOException e) {
                                        MipavUtil.displayError("IOException on image.importUData(0, intBuffer, true)");
                                        throw e;
                                    }
                                    break;
                                case ModelStorageBase.INTEGER:
                                    intBuffer = new int[totalNumber];
                                    try {
                                        image.exportData(0, totalNumber, intBuffer);
                                    }
                                    catch(IOException e) {
                                       MipavUtil.displayError("IOException on image.exportData(0, totalNumber, intBuffer)");
                                       throw e;
                                    }
                                    image.changeExtents(newExtents);
                                    image.recomputeDataSize();
                                    try {
                                        image.importData(0, intBuffer, true);
                                    }
                                    catch(IOException e) {
                                        MipavUtil.displayError("IOException on image.importData(0, intBuffer, true)");
                                        throw e;
                                    }
                                    break;
                                case ModelStorageBase.UINTEGER:
                                    longBuffer = new long[totalNumber];
                                    try {
                                        image.exportData(0, totalNumber, longBuffer);
                                    }
                                    catch(IOException e) {
                                       MipavUtil.displayError("IOException on image.exportData(0, totalNumber, longBuffer)");
                                       throw e;
                                    }
                                    image.changeExtents(newExtents);
                                    image.recomputeDataSize();
                                    try {
                                        image.importUData(0, longBuffer, true);
                                    }
                                    catch(IOException e) {
                                        MipavUtil.displayError("IOException on image.importUData(0, longBuffer, true)");
                                        throw e;
                                    }
                                    break;  
                                case ModelStorageBase.LONG:
                                    longBuffer = new long[totalNumber];
                                    try {
                                        image.exportData(0, totalNumber, longBuffer);
                                    }
                                    catch(IOException e) {
                                       MipavUtil.displayError("IOException on image.exportData(0, totalNumber, longBuffer)");
                                       throw e;
                                    }
                                    image.changeExtents(newExtents);
                                    image.recomputeDataSize();
                                    try {
                                        image.importData(0, longBuffer, true);
                                    }
                                    catch(IOException e) {
                                        MipavUtil.displayError("IOException on image.importData(0, longBuffer, true)");
                                        throw e;
                                    }
                                    break;
                                case ModelStorageBase.FLOAT:
                                    floatBuffer = new float[totalNumber];
                                    try {
                                        image.exportData(0, totalNumber, floatBuffer);
                                    }
                                    catch(IOException e) {
                                       MipavUtil.displayError("IOException on image.exportData(0, totalNumber, floatBuffer)");
                                       throw e;
                                    }
                                    image.changeExtents(newExtents);
                                    image.recomputeDataSize();
                                    try {
                                        image.importData(0, floatBuffer, true);
                                    }
                                    catch(IOException e) {
                                        MipavUtil.displayError("IOException on image.importData(0, floatBuffer, true)");
                                        throw e;
                                    }
                                    break;
                                case ModelStorageBase.DOUBLE:
                                    doubleBuffer = new double[totalNumber];
                                    try {
                                        image.exportData(0, totalNumber, doubleBuffer);
                                    }
                                    catch(IOException e) {
                                       MipavUtil.displayError("IOException on image.exportData(0, totalNumber, doubleBuffer)");
                                       throw e;
                                    }
                                    image.changeExtents(newExtents);
                                    image.recomputeDataSize();
                                    try {
                                        image.importData(0, doubleBuffer, true);
                                    }
                                    catch(IOException e) {
                                        MipavUtil.displayError("IOException on image.importData(0, doubleBuffer, true)");
                                        throw e;
                                    }
                                    break;
                                case ModelStorageBase.COMPLEX:
                                    realBuffer = new float[totalNumber];
                                    imaginaryBuffer = new float[totalNumber];
                                    try {
                                        image.exportComplexData(0, totalNumber, realBuffer, imaginaryBuffer);
                                    }
                                    catch(IOException e) {
                                       MipavUtil.displayError("IOException on image.exportComplexData(0, totalNumber, realBuffer, imaginaryBuffer)");
                                       throw e;
                                    }
                                    image.changeExtents(newExtents);
                                    image.recomputeDataSize();
                                    
                                    try {
                                        image.importComplexData(0, realBuffer, imaginaryBuffer, true, logMagDisplay);
                                    }
                                    catch(IOException e) {
                                       MipavUtil.displayError("IOException on image.importComplexData(0, realBuffer, imaginaryBuffer, true, logMagDisplay)");
                                       throw e;
                                    }       
                                    break;
                                case ModelStorageBase.DCOMPLEX:
                                    realDBuffer = new double[totalNumber];
                                    imaginaryDBuffer = new double[totalNumber];
                                    try {
                                        image.exportDComplexData(0, totalNumber, realDBuffer, imaginaryDBuffer);
                                    }
                                    catch(IOException e) {
                                       MipavUtil.displayError("IOException on image.exportDComplexData(0, totalNumber, realDBuffer, imaginaryDBuffer)");
                                       throw e;
                                    }
                                    image.changeExtents(newExtents);
                                    image.recomputeDataSize();
                                    
                                    try {
                                        image.importDComplexData(0, realDBuffer, imaginaryDBuffer, true, logMagDisplay);
                                    }
                                    catch(IOException e) {
                                       MipavUtil.displayError("IOException on image.importDComplexData(0, realDBuffer, imaginaryDBuffer, true, logMagDisplay)");
                                       throw e;
                                    }       
                                    break;
                                } // switch(image.getType())
                                fileInfo.setExtents(newExtents);
                            } // if (logicalFields >= 1)
                            image.calcMinMax();
                            fileInfo.setMin(image.getMin());
                            fileInfo.setMax(image.getMax());    
                        }
                        else {
                            if (logicalFields >= 1) {
                                switch(image2.getType()) {
                                case ModelStorageBase.BYTE:
                                    buffer = new byte[totalNumber];
                                    try {
                                        image2.exportData(0, totalNumber, buffer);
                                    }
                                    catch(IOException e) {
                                       MipavUtil.displayError("IOException on image2.exportData(0, totalNumber, buffer)");
                                       throw e;
                                    }
                                    image2.changeExtents(newExtents);
                                    image2.recomputeDataSize();
                                    try {
                                        image2.importData(0, buffer, true);
                                    }
                                    catch(IOException e) {
                                        MipavUtil.displayError("IOException on image2.importData(0, buffer, true)");
                                        throw e;
                                    }
                                    break;
                                case ModelStorageBase.UBYTE:
                                    shortBuffer = new short[totalNumber];
                                    try {
                                        image2.exportData(0, totalNumber, shortBuffer);
                                    }
                                    catch(IOException e) {
                                       MipavUtil.displayError("IOException on image2.exportData(0, totalNumber, shortBuffer)");
                                       throw e;
                                    }
                                    image2.changeExtents(newExtents);
                                    image2.recomputeDataSize();
                                    try {
                                        image2.importUData(0, shortBuffer, true);
                                    }
                                    catch(IOException e) {
                                        MipavUtil.displayError("IOException on image2.importUData(0, shortBuffer, true)");
                                        throw e;
                                    }
                                    break;
                                case ModelStorageBase.SHORT:
                                    shortBuffer = new short[totalNumber];
                                    try {
                                        image2.exportData(0, totalNumber, shortBuffer);
                                    }
                                    catch(IOException e) {
                                       MipavUtil.displayError("IOException on image2.exportData(0, totalNumber, shortBuffer)");
                                       throw e;
                                    }
                                    image2.changeExtents(newExtents);
                                    image2.recomputeDataSize();
                                    try {
                                        image2.importData(0, shortBuffer, true);
                                    }
                                    catch(IOException e) {
                                        MipavUtil.displayError("IOException on image2.importData(0, shortBuffer, true)");
                                        throw e;
                                    }
                                    break;
                                case ModelStorageBase.USHORT:
                                    intBuffer = new int[totalNumber];
                                    try {
                                        image2.exportData(0, totalNumber, intBuffer);
                                    }
                                    catch(IOException e) {
                                       MipavUtil.displayError("IOException on image2.exportData(0, totalNumber, intBuffer)");
                                       throw e;
                                    }
                                    image2.changeExtents(newExtents);
                                    image2.recomputeDataSize();
                                    try {
                                        image2.importUData(0, intBuffer, true);
                                    }
                                    catch(IOException e) {
                                        MipavUtil.displayError("IOException on image2.importUData(0, intBuffer, true)");
                                        throw e;
                                    }
                                    break;
                                case ModelStorageBase.INTEGER:
                                    intBuffer = new int[totalNumber];
                                    try {
                                        image2.exportData(0, totalNumber, intBuffer);
                                    }
                                    catch(IOException e) {
                                       MipavUtil.displayError("IOException on image2.exportData(0, totalNumber, intBuffer)");
                                       throw e;
                                    }
                                    image2.changeExtents(newExtents);
                                    image2.recomputeDataSize();
                                    try {
                                        image2.importData(0, intBuffer, true);
                                    }
                                    catch(IOException e) {
                                        MipavUtil.displayError("IOException on image2.importData(0, intBuffer, true)");
                                        throw e;
                                    }
                                    break;
                                case ModelStorageBase.UINTEGER:
                                    longBuffer = new long[totalNumber];
                                    try {
                                        image2.exportData(0, totalNumber, longBuffer);
                                    }
                                    catch(IOException e) {
                                       MipavUtil.displayError("IOException on image2.exportData(0, totalNumber, longBuffer)");
                                       throw e;
                                    }
                                    image2.changeExtents(newExtents);
                                    image2.recomputeDataSize();
                                    try {
                                        image2.importUData(0, longBuffer, true);
                                    }
                                    catch(IOException e) {
                                        MipavUtil.displayError("IOException on image2.importUData(0, longBuffer, true)");
                                        throw e;
                                    }
                                    break;  
                                case ModelStorageBase.LONG:
                                    longBuffer = new long[totalNumber];
                                    try {
                                        image2.exportData(0, totalNumber, longBuffer);
                                    }
                                    catch(IOException e) {
                                       MipavUtil.displayError("IOException on image2.exportData(0, totalNumber, longBuffer)");
                                       throw e;
                                    }
                                    image2.changeExtents(newExtents);
                                    image2.recomputeDataSize();
                                    try {
                                        image2.importData(0, longBuffer, true);
                                    }
                                    catch(IOException e) {
                                        MipavUtil.displayError("IOException on image2.importData(0, longBuffer, true)");
                                        throw e;
                                    }
                                    break;
                                case ModelStorageBase.FLOAT:
                                    floatBuffer = new float[totalNumber];
                                    try {
                                        image2.exportData(0, totalNumber, floatBuffer);
                                    }
                                    catch(IOException e) {
                                       MipavUtil.displayError("IOException on image2.exportData(0, totalNumber, floatBuffer)");
                                       throw e;
                                    }
                                    image2.changeExtents(newExtents);
                                    image2.recomputeDataSize();
                                    try {
                                        image2.importData(0, floatBuffer, true);
                                    }
                                    catch(IOException e) {
                                        MipavUtil.displayError("IOException on image2.importData(0, floatBuffer, true)");
                                        throw e;
                                    }
                                    break;
                                case ModelStorageBase.DOUBLE:
                                    doubleBuffer = new double[totalNumber];
                                    try {
                                        image2.exportData(0, totalNumber, doubleBuffer);
                                    }
                                    catch(IOException e) {
                                       MipavUtil.displayError("IOException on image2.exportData(0, totalNumber, doubleBuffer)");
                                       throw e;
                                    }
                                    image2.changeExtents(newExtents);
                                    image2.recomputeDataSize();
                                    try {
                                        image2.importData(0, doubleBuffer, true);
                                    }
                                    catch(IOException e) {
                                        MipavUtil.displayError("IOException on image2.importData(0, doubleBuffer, true)");
                                        throw e;
                                    }
                                    break;
                                case ModelStorageBase.COMPLEX:
                                    realBuffer = new float[totalNumber];
                                    imaginaryBuffer = new float[totalNumber];
                                    try {
                                        image2.exportComplexData(0, totalNumber, realBuffer, imaginaryBuffer);
                                    }
                                    catch(IOException e) {
                                       MipavUtil.displayError("IOException on image2.exportComplexData(0, totalNumber, realBuffer, imaginaryBuffer)");
                                       throw e;
                                    }
                                    image2.changeExtents(newExtents);
                                    image2.recomputeDataSize();
                                    
                                    try {
                                        image2.importComplexData(0, realBuffer, imaginaryBuffer, true, logMagDisplay);
                                    }
                                    catch(IOException e) {
                                       MipavUtil.displayError("IOException on image2.importComplexData(0, realBuffer, imaginaryBuffer, true, logMagDisplay)");
                                       throw e;
                                    }       
                                    break;
                                case ModelStorageBase.DCOMPLEX:
                                    realDBuffer = new double[totalNumber];
                                    imaginaryDBuffer = new double[totalNumber];
                                    try {
                                        image2.exportDComplexData(0, totalNumber, realDBuffer, imaginaryDBuffer);
                                    }
                                    catch(IOException e) {
                                       MipavUtil.displayError("IOException on image2.exportDComplexData(0, totalNumber, realDBuffer, imaginaryDBuffer)");
                                       throw e;
                                    }
                                    image2.changeExtents(newExtents);
                                    image2.recomputeDataSize();
                                    
                                    try {
                                        image2.importDComplexData(0, realDBuffer, imaginaryDBuffer, true, logMagDisplay);
                                    }
                                    catch(IOException e) {
                                       MipavUtil.displayError("IOException on image2.importDComplexData(0, realDBuffer, imaginaryDBuffer, true, logMagDisplay)");
                                       throw e;
                                    }       
                                    break;
                                } // switch(image.getType())
                                fileInfo2.setExtents(newExtents);   
                            }
                            image2.calcMinMax();
                            fileInfo2.setMin(image2.getMin());
                            fileInfo2.setMax(image2.getMax());
                        }
                        break;
                    case miUTF8:
                        Preferences.debug("Data type = miUTF8\n", Preferences.DEBUG_FILEIO);
                        Preferences.debug("Bytes in data element = " + elementBytes + "\n", Preferences.DEBUG_FILEIO);
                        buffer = new byte[elementBytes];
                        raFile.read(buffer);
                        str = new String(buffer, 0, elementBytes, "UTF-8");
                        Preferences.debug("UTF-8 encoded character data:\n" + str + "\n", Preferences.DEBUG_FILEIO);
                        break;
                    case miUTF16:
                        Preferences.debug("Data type = miUTF16\n", Preferences.DEBUG_FILEIO);
                        Preferences.debug("Bytes in data element = " + elementBytes + "\n", Preferences.DEBUG_FILEIO);
                        buffer = new byte[elementBytes];
                        raFile.read(buffer);
                        if (endianess == FileBase.BIG_ENDIAN) {
                            str = new String(buffer, 0, elementBytes, "UTF-16BE");
                        }
                        else {
                            str = new String(buffer, 0, elementBytes, "UTF-16LE");  
                        }
                        Preferences.debug("UTF-16 encoded character data:\n" + str + "\n", Preferences.DEBUG_FILEIO);
                        break;
                    case miUTF32:
                        Preferences.debug("Data type = miUTF32\n", Preferences.DEBUG_FILEIO);
                        Preferences.debug("Bytes in data element = " + elementBytes + "\n", Preferences.DEBUG_FILEIO);
                        buffer = new byte[elementBytes];
                        raFile.read(buffer);
                        if (endianess == FileBase.BIG_ENDIAN) {
                            str = new String(buffer, 0, elementBytes, "UTF-32BE");
                        }
                        else {
                            str = new String(buffer, 0, elementBytes, "UTF-32LE");  
                        }
                        Preferences.debug("UTF-32 encoded character data:\n" + str + "\n", Preferences.DEBUG_FILEIO);
                        break;
                    default:
                        Preferences.debug("Illegal data type = " + dataType + "\n", Preferences.DEBUG_FILEIO);
                        Preferences.debug("Bytes in data element = " + elementBytes + "\n", Preferences.DEBUG_FILEIO);
                    }
                    if (isCompressed) {
                        raFile.close();
                        try {
                            ufile.delete();
                        } catch (final SecurityException sc) {
                            MipavUtil.displayError("Security error occurs while trying to delete " + uncompressedName);
                        }
                        raFile = new RandomAccessFile(file, "r");
                    }
                } // while (nextElementAddress)
                raFile.close();
                if (zlibDecompresser != null) {
                    zlibDecompresser.end();
                    zlibDecompresser = null;
                }
                
               
                if (fileInfo.getArrayName() != null) {
                    image.setImageName(fileInfo.getArrayName());
                    fileInfo.setFileName(fileInfo.getArrayName());
                }
                fileInfo.setSourceFile(fileDir + fileName);
                image.setFileInfo((FileInfoMATLAB)fileInfo.clone(), 0);
                /*for (i = 0; i < imageSlices; i++) {
                    
                    image.setFileInfo((FileInfoMATLAB)fileInfo.clone(), i);
                }*/
                
                if (((image.getType() == ModelStorageBase.UBYTE)|| (image.getType() == ModelStorageBase.SHORT)) &&
                    (image2 != null) && (image2.getType() == ModelStorageBase.DOUBLE) &&
                    (image2.getExtents()[0]  == 3)  && (image2.getExtents()[1] <= 256)) {
                    // image2 is really a lookup table for image
                    totalNumber = 1;
                    for (i = 0; i < image.getNDims(); i++) {
                        totalNumber = totalNumber * image.getExtents()[i];
                    }
                    shortBuffer = new short[totalNumber];
                    try {
                        image.exportData(0, totalNumber, shortBuffer);
                    }
                    catch(IOException e) {
                       MipavUtil.displayError("IOException on image.exportData(0, totalNumber, shortBuffer)");
                       throw e;
                    }
                    shortMin = 65535;
                    shortMax = -32768;
                    for (i = 0; i < totalNumber; i++) {
                        if (shortBuffer[i] < shortMin) {
                            shortMin = shortBuffer[i];
                        }
                        if (shortBuffer[i] > shortMax) {
                            shortMax = shortBuffer[i];
                        }
                    }
                    
                    shortRange = shortMax - shortMin;
                    image = new ModelImage(ModelStorageBase.ARGB_FLOAT, image.getExtents(), image.getImageFileName());
                    fileInfo.setDataType(ModelStorageBase.ARGB_FLOAT);
                    floatBuffer = new float[4 * totalNumber];
                    totalNumber2 = 1;
                    for (i = 0; i < image2.getNDims(); i++) {
                        totalNumber2 = totalNumber2 * image2.getExtents()[i];
                    }
                    doubleBuffer = new double[totalNumber2];
                    maxColorIndex = (totalNumber2/3) - 1;
                    scale = (double)maxColorIndex/(double)shortRange;
                    try {
                        image2.exportData(0, totalNumber2, doubleBuffer);
                    }
                    catch(IOException e) {
                       MipavUtil.displayError("IOException on image2.exportData(0, totalNumber2, doubleBuffer)");
                       throw e;
                    }
                    
                    image2.disposeLocal();
                    image2 = null;
                    for (i = 0; i < totalNumber; i++) {
                        floatBuffer[4*i] = 0.0f;
                        scaledIndex = (scale*(shortBuffer[i] - shortMin));
                        lowIndex = Math.max(0,(int)(scale*(shortBuffer[i] - shortMin)));
                        highIndex = Math.min(maxColorIndex,lowIndex+1);
                        if (lowIndex == highIndex) {
                            lowFraction = 1.0;
                            highFraction = 0.0;
                        }
                        else {
                            lowFraction = Math.min(1.0, highIndex - scaledIndex);
                            highFraction = Math.min(1.0, scaledIndex - lowIndex);
                        }
                        floatBuffer[4*i+1] = (float)(255*(lowFraction*doubleBuffer[3*lowIndex] + highFraction*doubleBuffer[3*highIndex]));
                        floatBuffer[4*i+2] = (float)(255*(lowFraction*doubleBuffer[3*lowIndex+1] + highFraction*doubleBuffer[3*highIndex+1]));
                        floatBuffer[4*i+3] = (float)(255*(lowFraction*doubleBuffer[3*lowIndex+2] + highFraction*doubleBuffer[3*highIndex+2]));
                    }
                    try {
                        image.importData(0, floatBuffer, true);
                    }
                    catch(IOException e) {
                        MipavUtil.displayError("IOException on image.importData(0, floatBuffer, true)");
                        throw e;
                    }
                }
                else if ((image.getType() == ModelStorageBase.DOUBLE) && (image.getExtents()[0]  == 3)  && (image.getExtents()[1] <= 256) &&
                         (image2 != null) && ((image2.getType() == ModelStorageBase.UBYTE) || (image2.getType() == ModelStorageBase.SHORT))) {
                        // image2 is really a lookup table for image
                        totalNumber = 1;
                        for (i = 0; i < image2.getNDims(); i++) {
                            totalNumber = totalNumber * image2.getExtents()[i];
                        }
                        shortBuffer = new short[totalNumber];
                        try {
                            image2.exportData(0, totalNumber, shortBuffer);
                        }
                        catch(IOException e) {
                           MipavUtil.displayError("IOException on image2.exportData(0, totalNumber, shortBuffer)");
                           throw e;
                        }
                        shortMin = 65535;
                        shortMax = -32768;
                        for (i = 0; i < totalNumber; i++) {
                            if (shortBuffer[i] < shortMin) {
                                shortMin = shortBuffer[i];
                            }
                            if (shortBuffer[i] > shortMax) {
                                shortMax = shortBuffer[i];
                            }
                        }
                        shortRange = shortMax - shortMin;
                        floatBuffer = new float[4 * totalNumber];
                        totalNumber2 = 1;
                        for (i = 0; i < image.getNDims(); i++) {
                            totalNumber2 = totalNumber2 * image.getExtents()[i];
                        }
                        doubleBuffer = new double[totalNumber2];
                        maxColorIndex = (totalNumber2/3) - 1;
                        scale = (double)maxColorIndex/(double)shortRange;
                        try {
                            image.exportData(0, totalNumber2, doubleBuffer);
                        }
                        catch(IOException e) {
                           MipavUtil.displayError("IOException on image.exportData(0, totalNumber2, doubleBuffer)");
                           throw e;
                        }
                        
                        image = new ModelImage(ModelStorageBase.ARGB_FLOAT, image2.getExtents(), image2.getImageFileName());
                        fileInfo = (FileInfoMATLAB)fileInfo2.clone();
                        fileInfo.setDataType(ModelStorageBase.ARGB_FLOAT);
                        image2.disposeLocal();
                        image2 = null;
                        for (i = 0; i < totalNumber; i++) {
                            floatBuffer[4*i] = 0.0f;
                            scaledIndex = (scale*(shortBuffer[i] - shortMin));
                            lowIndex = Math.max(0,(int)(scale*(shortBuffer[i] - shortMin)));
                            highIndex = Math.min(maxColorIndex,lowIndex+1);
                            if (lowIndex == highIndex) {
                                lowFraction = 1.0;
                                highFraction = 0.0;
                            }
                            else {
                                lowFraction = Math.min(1.0, highIndex - scaledIndex);
                                highFraction = Math.min(1.0, scaledIndex - lowIndex);
                            }
                            floatBuffer[4*i+1] = (float)(255*(lowFraction*doubleBuffer[3*lowIndex] + highFraction*doubleBuffer[3*highIndex]));
                            floatBuffer[4*i+2] = (float)(255*(lowFraction*doubleBuffer[3*lowIndex+1] + highFraction*doubleBuffer[3*highIndex+1]));
                            floatBuffer[4*i+3] = (float)(255*(lowFraction*doubleBuffer[3*lowIndex+2] + highFraction*doubleBuffer[3*highIndex+2]));
                        }
                        try {
                            image.importData(0, floatBuffer, true);
                        }
                        catch(IOException e) {
                            MipavUtil.displayError("IOException on image.importData(0, floatBuffer, true)");
                            throw e;
                        }
                    }
                    
                
                
                if (image2 != null) {
                    fileInfo2.setSourceFile(fileDir + fileName);
                    if (fileInfo2.getArrayName() != null) {
                        image2.setImageName(fileInfo2.getArrayName());
                        fileInfo2.setFileName(fileInfo2.getArrayName());
                    }
                     for (i = 0; i < imageSlices2; i++) {
                         
                         image2.setFileInfo((FileInfoMATLAB)fileInfo2.clone(), i);
                     }
                     vFrame2 = new ViewJFrameImage(image2);
                     if (fileInfo2.getArrayName() != null) {
                         vFrame2.setTitle(fileInfo2.getArrayName());
                     }
                     else {
                         vFrame2.setTitle(fileName.substring(0,st) + "_2");
                     }
                }
                
                if (maskImage != null) {
                    if (!isVOI) {
                        // If xDim or yDim of maskImage does not equal xDim or yDim of image,
                        // then do not convert to a VOI
                        maskImage.setFileInfo(maskFileInfo, 0);
                        vmFrame = new ViewJFrameImage(maskImage);
                        if (voiFieldName != null) {
                            vmFrame.setTitle(voiFieldName);
                        }
                        else {
                            vmFrame.setTitle(fileName.substring(0,st) + "_mask");
                        }
                    } // if (!isVOI)
                    else { // convert maskImage to VOI
                        if ((image.getNDims() >= 3) && (maskImage.getNDims() == 2)) {
                            // Convert maskImage from 2D to 3D
                            sliceSize = image.getExtents()[0]*image.getExtents()[1];
                            newExtents = new int[3];
                            for (i = 0; i < 3; i++) {
                                newExtents[i] = image.getExtents()[i];
                            }
                            switch(maskImage.getType()) {
                            case ModelStorageBase.BYTE:
                                buffer = new byte[sliceSize];
                                try {
                                    maskImage.exportData(0, sliceSize, buffer);
                                }
                                catch(IOException e) {
                                   MipavUtil.displayError("IOException on maskImage.exportData(0, sliceSize, buffer)");
                                   throw e;
                                }
                                maskImage.changeExtents(newExtents);
                                maskImage.recomputeDataSize();
                                for (i = 0; i < image.getExtents()[2]; i++) {
                                    try {
                                        maskImage.importData(i * sliceSize, buffer, false);
                                    }
                                    catch(IOException e) {
                                        MipavUtil.displayError("IOException on maskImage.importData(i * sliceSize, buffer, false)");
                                        throw e;
                                    }
                                }
                                maskImage.calcMinMax();
                                break;
                            case ModelStorageBase.UBYTE:
                                shortBuffer = new short[sliceSize];
                                try {
                                    maskImage.exportData(0, sliceSize, shortBuffer);
                                }
                                catch(IOException e) {
                                   MipavUtil.displayError("IOException on maskImage.exportData(0, sliceSize, shortBuffer)");
                                   throw e;
                                }
                                maskImage.changeExtents(newExtents);
                                maskImage.recomputeDataSize();
                                for (i = 0; i < image.getExtents()[2]; i++) {
                                    try {
                                        maskImage.importUData(i * sliceSize, shortBuffer, false);
                                    }
                                    catch(IOException e) {
                                        MipavUtil.displayError("IOException on maskImage.importUData(i * sliceSize, shortBuffer, false)");
                                        throw e;
                                    }
                                }
                                maskImage.calcMinMax();
                                break;
                            }
                        } // if ((image.getNDims() >= 3) && (maskImage.getNDims() == 2))
                        boolean wholeImage = true;
                        if (maskImage.getNDims() == 2) { 
                            AlgorithmMorphology2D idObjectsAlgo2D;
                            int method = AlgorithmMorphology2D.ID_OBJECTS;
                            
                            idObjectsAlgo2D = new AlgorithmMorphology2D(maskImage, 0, 0, method, 0, 0, 0, 0, wholeImage);
                            idObjectsAlgo2D.setMinMax(1, Integer.MAX_VALUE);
                            idObjectsAlgo2D.run();
                            idObjectsAlgo2D.finalize();
                            idObjectsAlgo2D = null;
                        }
                        else { 
                            AlgorithmMorphology3D idObjectsAlgo3D;
                            int method = AlgorithmMorphology3D.ID_OBJECTS;
                            
                            idObjectsAlgo3D = new AlgorithmMorphology3D(maskImage, 0, 0, method, 0, 0, 0, 0, wholeImage);
                            idObjectsAlgo3D.setMinMax(1, Integer.MAX_VALUE);
                            idObjectsAlgo3D.run();
                            idObjectsAlgo3D.finalize();
                            idObjectsAlgo3D = null;
                        }
                        maskImage.calcMinMax();
                        final AlgorithmVOIExtraction VOIExtractionAlgo = new AlgorithmVOIExtraction(maskImage);

                        // VOIExtractionAlgo.setActiveImage(false);
                        VOIExtractionAlgo.run();
                        
                        VOIVector kVOIs = maskImage.getVOIs();
                        for (i = 0; i < kVOIs.size(); i++ )
                        {
                            VOI kCurrentGroup = kVOIs.get(i);
                            kCurrentGroup.setAllActive(true);
                        }
                        maskImage.groupVOIs();
                        kVOIs = maskImage.getVOIs();
                        if (voiFieldName != null) {
                            ((VOI)kVOIs.get(0)).setName(voiFieldName);
                        }
                        image.setVOIs(kVOIs);
                        maskImage.disposeLocal();
                        maskImage = null;
                    } // convert maskImage to VOI
                }
                
                if (maskImage2 != null) {
                    if (!isVOI2) {
                        // If xDim or yDim of maskImage2 does not equal xDim or yDim of image2,
                        // then do not convert to a VOI
                        maskImage2.setFileInfo(maskFileInfo2, 0);
                        vmFrame2 = new ViewJFrameImage(maskImage2);
                        if (voi2FieldName != null) {
                            vmFrame2.setTitle(voi2FieldName);
                        }
                        else {
                            vmFrame2.setTitle(fileName.substring(0,st) + "_2_mask");
                        }
                    } // if (!isVOI2)
                    else { // convert maskImage2 to VOI
                        if ((image2.getNDims() >= 3) && (maskImage2.getNDims() == 2)) {
                            // Convert maskImage2 from 2D to 3D
                            sliceSize = image2.getExtents()[0]*image2.getExtents()[1];
                            newExtents = new int[3];
                            for (i = 0; i < 3; i++) {
                                newExtents[i] = image2.getExtents()[i];
                            }
                            switch(maskImage2.getType()) {
                            case ModelStorageBase.BYTE:
                                buffer = new byte[sliceSize];
                                try {
                                    maskImage2.exportData(0, sliceSize, buffer);
                                }
                                catch(IOException e) {
                                   MipavUtil.displayError("IOException on maskImage2.exportData(0, sliceSize, buffer)");
                                   throw e;
                                }
                                maskImage2.changeExtents(newExtents);
                                maskImage2.recomputeDataSize();
                                for (i = 0; i < image2.getExtents()[2]; i++) {
                                    try {
                                        maskImage2.importData(i * sliceSize, buffer, false);
                                    }
                                    catch(IOException e) {
                                        MipavUtil.displayError("IOException on maskImage2.importData(i * sliceSize, buffer, false)");
                                        throw e;
                                    }
                                }
                                maskImage2.calcMinMax();
                                break;
                            case ModelStorageBase.UBYTE:
                                shortBuffer = new short[sliceSize];
                                try {
                                    maskImage2.exportData(0, sliceSize, shortBuffer);
                                }
                                catch(IOException e) {
                                   MipavUtil.displayError("IOException on maskImage2.exportData(0, sliceSize, shortBuffer)");
                                   throw e;
                                }
                                maskImage2.changeExtents(newExtents);
                                maskImage2.recomputeDataSize();
                                for (i = 0; i < image2.getExtents()[2]; i++) {
                                    try {
                                        maskImage2.importUData(i * sliceSize, shortBuffer, false);
                                    }
                                    catch(IOException e) {
                                        MipavUtil.displayError("IOException on maskImage2.importUData(i * sliceSize, shortBuffer, false)");
                                        throw e;
                                    }
                                }
                                maskImage2.calcMinMax();
                                break;
                            }
                        } // if ((image.getNDims() >= 3) && (maskImage.getNDims() == 2))
                        boolean wholeImage = true;
                        if (maskImage2.getNDims() == 2) { 
                            AlgorithmMorphology2D idObjectsAlgo2D;
                            int method = AlgorithmMorphology2D.ID_OBJECTS;
                            
                            idObjectsAlgo2D = new AlgorithmMorphology2D(maskImage2, 0, 0, method, 0, 0, 0, 0, wholeImage);
                            idObjectsAlgo2D.setMinMax(1, Integer.MAX_VALUE);
                            idObjectsAlgo2D.run();
                            idObjectsAlgo2D.finalize();
                            idObjectsAlgo2D = null;
                        }
                        else { 
                            AlgorithmMorphology3D idObjectsAlgo3D;
                            int method = AlgorithmMorphology3D.ID_OBJECTS;
                            
                            idObjectsAlgo3D = new AlgorithmMorphology3D(maskImage2, 0, 0, method, 0, 0, 0, 0, wholeImage);
                            idObjectsAlgo3D.setMinMax(1, Integer.MAX_VALUE);
                            idObjectsAlgo3D.run();
                            idObjectsAlgo3D.finalize();
                            idObjectsAlgo3D = null;
                        }
                        maskImage2.calcMinMax();
                        final AlgorithmVOIExtraction VOIExtractionAlgo = new AlgorithmVOIExtraction(maskImage2);

                        // VOIExtractionAlgo.setActiveImage(false);
                        VOIExtractionAlgo.run();
                        
                        VOIVector kVOIs = maskImage2.getVOIs();
                        for (i = 0; i < kVOIs.size(); i++ )
                        {
                            VOI kCurrentGroup = kVOIs.get(i);
                            kCurrentGroup.setAllActive(true);
                        }
                        maskImage2.groupVOIs();
                        kVOIs = maskImage2.getVOIs();
                        if (voi2FieldName != null) {
                            ((VOI)kVOIs.get(0)).setName(voi2FieldName);
                        }
                        image2.setVOIs(kVOIs);
                        maskImage2.disposeLocal();
                        maskImage2 = null;
                    } // convert maskImage2 to VOI
                }
                
                fireProgressStateChanged(100);
                
            } catch (OutOfMemoryError error) {

                if (image != null) {
                    image.disposeLocal();
                    image = null;
                }
                
                if (image2 != null) {
                    image2.disposeLocal();
                    image2 = null;
                }

                System.gc();
                throw error;
            }

            return image;
        }
        
        private String readCString() throws IOException {
            String cString = "";
            boolean nullFound = false;
            byte oneByte[] = new byte[1];
            while (!nullFound) {
                raFile.read(oneByte);
                if (oneByte[0]  == 0) {
                    nullFound = true;
                }
                else {
                    cString += new String(oneByte);
                }
            } // while (!nullFound)
            return cString;
        }

        
        /**
         * Accessor to set the file name (used for reading COR multiFile).
         *
         * @param  fName  file name of image to read.
         */
        public void setFileName(String fName) {
            fileName = fName;
        }
        
        

        
    }


    

    /**
     * Starts the algorithm.
     */
    public void runAlgorithm() {
        return;
    }
    
    
    
}