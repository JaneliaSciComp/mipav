package gov.nih.mipav.view.renderer.WildMagic.DTI_FrameWork;

import gov.nih.mipav.model.algorithms.AlgorithmInterface;

import java.awt.Color;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;
import java.io.File;
import java.io.IOException;
import java.util.ArrayList;

import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JFileChooser;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTextField;
import javax.swing.border.EtchedBorder;
import javax.swing.border.TitledBorder;

import Jama.Matrix;

import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.file.FileIO;
import gov.nih.mipav.model.file.FileInfoBase;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.TransMatrix;

import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.Preferences;
import gov.nih.mipav.view.ViewImageFileFilter;
import gov.nih.mipav.view.ViewJFrameImage;
import gov.nih.mipav.view.ViewUserInterface;
import gov.nih.mipav.view.renderer.WildMagic.Interface.JInterfaceBase;

public class JPanelEPIDistortionCorrection extends JPanel implements AlgorithmInterface, ActionListener, ItemListener {
    // ~ Instance fields
    // ------------------------------------------------------------------------------------------------

    private DTIPipeline pipeline;

    /** DOCUMENT ME! */
    private ViewUserInterface UI;
    
    private JButton TestButton;
    
    /** grid bag constraints * */
    private GridBagConstraints gbc;

    private Font serif12;

    private Font serif12B;
    
    public JPanel mainEPIPanel;
    
    private JPanelDTIPreprocessing DTIPreprocessing;
    
    private ModelImage[] EPI4dto3dArray;
    
    private float currentDef[][][][];
    
    private ModelImage EPI4dto3dVolume;
    
    private ModelImage DefImage;
    
    private int XN, YN, ZN,TN, chN;

    float[] dimRes;
    
    private int[] epiExtents;

    private JTextField textDefimage;

    private ModelImage newDefImage;

    private int[] newDefExtents;
    
    public JPanelEPIDistortionCorrection(DTIPipeline pipeline) {
        super();
        // super(theParentFrame, false);
        // super();
        // matchB0image = im;

        this.pipeline = pipeline;

        UI = ViewUserInterface.getReference();
        init();
    }
    
    // ~ Methods
    // --------------------------------------------------------------------------------------------------------

    /**
     * Closes dialog box when the OK button is pressed, sets the variables, and calls the algorithm.
     * 
     * @param event Event that triggers function.
     */
    public void actionPerformed(ActionEvent event) {
        String command = event.getActionCommand();
        String tmpStr;
        if ( command.equals("browseDefFile")){
                loadDefImage();
            }
        if (command.equals("TEST")){ 
            if(pipeline.DWINewB0Image !=null && DefImage!= null){
                EPIAlgorithm();
            }
        }
        
        }
    
    // ************************************************************************
    // ************************** Algorithm Events ****************************
    // ************************************************************************

    /**
     * This method is required if the AlgorithmPerformed interface is implemented. It is called by the algorithms when
     * it has completed or failed to complete.
     * 
     * @param algorithm Algorithm that caused the event.
     */
    public void algorithmPerformed(AlgorithmBase algorithm) {

    }
    
    private TitledBorder buildTitledBorder(String title) {
        return new TitledBorder(new EtchedBorder(), title, TitledBorder.LEFT, TitledBorder.CENTER, MipavUtil.font12B,
                Color.black);
    }

    /**
     * Initializes the GUI components and displays the dialog.
     */
    private void init() {
        setForeground(Color.black);

        JPanel buttonPanel = new JPanel();
        buildTestButton();
        buttonPanel.add(TestButton);
        
        Insets insets = new Insets(0, 2, 0, 2);
        GridBagConstraints gbc = new GridBagConstraints();

        gbc.insets = insets;
        
        JPanel DTIloadPanel = new JPanel(new GridBagLayout());
        DTIloadPanel.setBorder(JInterfaceBase.buildTitledBorder("Upload Def Image"));            
        JLabel maskLabel = new JLabel("Def Field: ");
        maskLabel.setFont(serif12);
        maskLabel.setEnabled(true);
        gbc.gridx = 0;
        gbc.gridy = 1;
        gbc.weightx = 1;
        gbc.gridwidth = 1;
        gbc.gridheight = 1;
        gbc.weightx = 0;
        gbc.fill = GridBagConstraints.NONE;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.insets = new Insets(0, 2, 0, 2);
        DTIloadPanel.add(maskLabel,gbc);
        
        textDefimage = new JTextField();
        textDefimage.setPreferredSize(new Dimension(100, 21));
        textDefimage.setEnabled(true);
        textDefimage.setBackground(Color.white);
        textDefimage.setFont(MipavUtil.font12);
        gbc.gridx = 1;
        gbc.gridy = 1;
        gbc.weightx = 0.15;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        DTIloadPanel.add(textDefimage,gbc);
        
        
        JButton openMaskImageButton = new JButton("Browse");
        openMaskImageButton.setToolTipText("Browse mask image file");
        openMaskImageButton.addActionListener(this);
        openMaskImageButton.setActionCommand("browseDefFile");
        openMaskImageButton.setEnabled(true);
        gbc.gridx = 2;
        gbc.gridy = 1;
        gbc.weightx = 0.25;
        gbc.fill = GridBagConstraints.NONE;
        DTIloadPanel.add(openMaskImageButton,gbc);
        
        mainEPIPanel = new JPanel();
        mainEPIPanel.setLayout(new GridBagLayout());

        

        // gbc.anchor = GridBagConstraints.NORTHWEST;
        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.weightx = .5;
        gbc.weighty = 0;
        gbc.gridwidth = 1;
        gbc.fill = GridBagConstraints.BOTH;

        mainEPIPanel.add(DTIloadPanel, gbc);
        gbc.gridx = 0;
        gbc.gridy = 1;
        gbc.weightx = .5;
        gbc.weighty = 0;
        gbc.gridwidth = 1;
        gbc.fill = GridBagConstraints.BOTH;
        mainEPIPanel.add(buttonPanel, gbc);
        
        setVisible(true);

    }
       
    private JButton buildTestButton() {
        TestButton = new JButton("TEST");
        TestButton.addActionListener(this);

        // OKButton.setToolTipText("Accept values and perform action.");
        TestButton.setMinimumSize(MipavUtil.defaultButtonSize);
        TestButton.setPreferredSize(MipavUtil.defaultButtonSize);
        TestButton.setFont(serif12B);

        return TestButton;
    }
    
    private void loadDefImage() {
        final JFileChooser chooser = new JFileChooser(new File(Preferences.getProperty(Preferences.PREF_IMAGE_DIR)));
        chooser.addChoosableFileFilter(new ViewImageFileFilter(ViewImageFileFilter.TECH));
        chooser.setDialogTitle("Choose Deformation field file");
        final int returnValue = chooser.showOpenDialog(this);
        if (returnValue == JFileChooser.APPROVE_OPTION) {
            final FileIO fileIO = new FileIO();

            DefImage = fileIO.readImage(chooser.getSelectedFile().getName(), chooser.getCurrentDirectory()
                    + File.separator);
            textDefimage.setText(chooser.getSelectedFile().getAbsolutePath());
            Preferences.setProperty(Preferences.PREF_IMAGE_DIR, chooser.getCurrentDirectory().toString());
        }
    }
    
    private void EPIAlgorithm(){
        XN = pipeline.DWINewB0Image.getExtents()[0];
        YN = pipeline.DWINewB0Image.getExtents()[1];
        ZN = pipeline.DWINewB0Image.getExtents()[2];
        TN = pipeline.DWINewB0Image.getExtents()[3];
        chN = 3;
        dimRes = DefImage.getResolutions(0);
        int sliceSize = XN * YN;

        epiExtents = new int[3];
        epiExtents[0] = pipeline.DWINewB0Image.getExtents()[0];
        epiExtents[1] = pipeline.DWINewB0Image.getExtents()[1];
        epiExtents[2] = pipeline.DWINewB0Image.getExtents()[2];
        
        newDefExtents = new int[4];
        newDefExtents[0] = pipeline.DWINewB0Image.getExtents()[0];
        newDefExtents[1] = pipeline.DWINewB0Image.getExtents()[1];
        newDefExtents[2] = pipeline.DWINewB0Image.getExtents()[2];
        newDefExtents[3] = 3;
        
        EPI4dto3dArray = new ModelImage[chN];
        
              
        for (int i = 0; i < chN; i++){
            //Allocate VABRA out deformation field into an array of ModelImages
            EPI4dto3dArray[i] = new ModelImage(DefImage.getType(), epiExtents, "EPI4dto3dArray");
            try {
                float[] imageBuffer = new float[sliceSize*ZN];
                DefImage.exportData(i*sliceSize*ZN, sliceSize*ZN, imageBuffer);
                EPI4dto3dArray[i].importData(0, imageBuffer, false);
                imageBuffer = null;
                
            }catch (IOException error) {
                Preferences.debug("! ERROR: " + "EPI4dto3dArray" + "\n", Preferences.DEBUG_ALGORITHM);;
                return;
            }
        }
        

    Matrix m;
    TransMatrix matarray;
    Matrix b0toStruct;
    b0toStruct = new Matrix(4,4);
    
    
    for (int i = 0; i < pipeline.arrayTransMatrix.length-1; i++){                     
            m = new Matrix(4,4);
                for(int j=0; j<4; j++){
                    for(int k=0; k<4; k++){   
                        m.set(j, k, pipeline.arrayTransMatrix[i].Get(j, k));
                        b0toStruct.set(j, k, pipeline.b0toStructMatrix.Get(j, k));                           

                    }                   
                }
                //Multiple B0 to Struct trans matrix from trans matrix corresponding to DWI volume
                m = b0toStruct.times(m);
               
                currentDef=createDefFieldFromTransMatrix(m);
                currentDef=applyDeformation(currentDef, EPI4dto3dArray);
                currentDeftoNewDefImg(currentDef);

        }
    
    
    
    }
    
    public float[][][][] applyDeformation(float[][][][] currentDef, ModelImage[] epiDef){

        double[] currentVec = new double[3];
        double[] newVec = new double[3];
        
        float[][][][] newDef = new float[chN][XN][YN][ZN];

            for(int i = 0; i < XN; i++) {
                for(int j = 0; j < YN; j++) {
                    for(int k = 0; k < ZN; k++){
                        for (int c = 0; c < chN; c++) {
                            newDef[c][i][j][k] = currentDef[c][i][j][k]; 
                    }
                }
            }
        }
        /*System.out.println("newDef2[0][127][127][10]" +newDef[0][127][127][10]);
        System.out.println("newDef2[1][127][127][10]" +newDef[1][127][127][10]);*/
        

        for(int i = 0; i < XN; i++)
            for(int j = 0; j < YN; j++)
                for(int k = 0; k < ZN; k++){
                    
                    for(int c = 0; c < chN; c++){
                     newVec[c] = epiDef[c].getDouble(i, j, k); 
                     }
                    
                   //get current deformation at where the new deformation is pointing
                    for(int c = 0; c < chN; c++) currentVec[c] = TrilinearInterpolation(currentDef[c], XN, YN, ZN, 
                            newVec[0]+i, newVec[1]+j, newVec[2]+k); 
                    
                    for(int c = 0; c < chN; c++){
                     newDef[c][i][j][k]= (float) (newVec[c]+currentVec[c]);
                     }
                    
                }

        return newDef;          
        
    }
    
    public float[][][][] createDefFieldFromTransMatrix(Matrix newTrans){
        double[] currentVec = new double[3];
        double[] newVec = new double[3];
        float[][][][] newDef = new float[chN][XN][YN][ZN];
;
        //System.out.println("dimRes[0]" +dimRes[0]);
        //System.out.println("dimRes[1]" +dimRes[1]);
        //System.out.println("dimRes[2]" +dimRes[2]);
            for(int i = 0; i < XN; i++){
                for(int j = 0; j < YN; j++){
                    for(int k = 0; k < ZN; k++){ 
                    //We need to make sure resolutions on VABRA deformation field are being set correctly
                    newVec[0] = i*dimRes[0];
                    newVec[1] = j*dimRes[1];
                    newVec[2] = k*dimRes[2];
                    
                                        
                    Matrix v = new Matrix(new double[]{newVec[0],newVec[1],newVec[2],1},4);
                    
                    Matrix vp = newTrans.solve(v);

                    
                    for(int ch = 0; ch < 3; ch++) {
                        newVec[ch] = vp.get(ch,0) - newVec[ch];
                        newDef[ch][i][j][k] = (float)( newVec[ch]/dimRes[ch]);
                        }
                    
                    }
                }
            
        }
            /*System.out.println("newDef[0][127][127][10]" +newDef[0][127][127][10]);
            System.out.println("newDef[1][127][127][10]" +newDef[1][127][127][10]);*/
        
        return newDef;
        
    }
    
    public void currentDeftoNewDefImg(float[][][][] currentDef){
        float imageDefBuffer[] = new float[XN*YN*ZN*chN];
        ArrayList<Float> arrayListBuffer = new ArrayList<Float>() ;
        int counter = 0;
        for(int p = 0; p < XN; p++) {
            for(int j = 0; j < YN; j++) {
                for(int k = 0; k < ZN; k++){
                    for (int c = 0; c < chN; c++) {
                        //Make quadruple float array of deformation field into one large array list
                        arrayListBuffer.add(counter,(float)currentDef[c][p][j][k]);
                        counter++;
                    }
                }
            }
        }

        for(int i=0; i< arrayListBuffer.size(); i++){
            imageDefBuffer[i] = arrayListBuffer.get(i);
        }

        newDefImage = new ModelImage(DefImage.getType(), newDefExtents, ("NewDefImage"));
        try {                    
            newDefImage.importData(0, imageDefBuffer, false) ;              
        } catch (IOException e) {
        // TODO Auto-generated catch block
        e.printStackTrace();
        }
        
        /*Need to add fileInfo to new deformation field images once algorithm is working correctly
         * for (int t = 0; t < TN-1; t++){
         

               /* if (pipeline.DWINewB0Image.isDicomImage() != true ){
                    FileInfoBase fileInfoBuffer; // buffer of any old type
                    fileInfoBuffer = (FileInfoBase) pipeline.DWINewB0Image.getFileInfo(t).clone();
                    fileInfoBuffer.setExtents(pipeline.DWINewB0Image.getExtents());
                    fileInfoBuffer.setResolutions(pipeline.DWINewB0Image.getFileInfo(0).getResolutions());
                    EPI4dto3dArray[t].setFileInfo(fileInfoBuffer, t);
                }*/
           
        //}
        
        
        if (newDefImage != null) {
            try {
                new ViewJFrameImage(newDefImage, null, new Dimension(610, 200));
            } catch (final OutOfMemoryError error) {
                MipavUtil.displayError("Out of memory: unable to open new frame");
            }
        }
              
    }
    

    
    public double TrilinearInterpolation(float[][][] oldV, int XN, int YN,
            int ZN, double x, double y, double z) {
        //Bill wrote this method
        int i0, j0, k0, i1, j1, k1;
        double dx, dy, dz, hx, hy, hz;
        if (x < 0 || x > (XN - 1) || y < 0 || y > (YN - 1) || z < 0
                || z > (ZN - 1)) {
            return 0;
        } else {
            j1 = (int) Math.ceil(x);
            i1 = (int) Math.ceil(y);
            k1 = (int) Math.ceil(z);
            j0 = (int) Math.floor(x);
            i0 = (int) Math.floor(y);
            k0 = (int) Math.floor(z);
            dx = x - j0;
            dy = y - i0;
            dz = z - k0;

            // Introduce more variables to reduce computation
            hx = 1.0 - dx;
            hy = 1.0 - dy;
            hz = 1.0 - dz;
            // Optimized below
            return   (((oldV[j0][i0][k0] * hx + oldV[j1][i0][k0] * dx) * hy 
                     + (oldV[j0][i1][k0] * hx + oldV[j1][i1][k0] * dx) * dy) * hz 
                    + ((oldV[j0][i0][k1] * hx + oldV[j1][i0][k1] * dx) * hy 
                     + (oldV[j0][i1][k1] * hx + oldV[j1][i1][k1] * dx) * dy)* dz);

        }
    }
    
  

    @Override
    public void itemStateChanged(ItemEvent e) {
        // TODO Auto-generated method stub
        
    }

}
