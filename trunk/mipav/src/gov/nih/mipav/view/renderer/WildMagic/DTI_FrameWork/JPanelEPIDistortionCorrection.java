package gov.nih.mipav.view.renderer.WildMagic.DTI_FrameWork;

import gov.nih.mipav.model.algorithms.AlgorithmInterface;

import java.awt.Color;
import java.awt.Font;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;
import java.io.IOException;

import javax.swing.JButton;
import javax.swing.JPanel;
import javax.swing.border.EtchedBorder;
import javax.swing.border.TitledBorder;

import Jama.Matrix;

import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.file.FileInfoBase;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.TransMatrix;

import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.Preferences;
import gov.nih.mipav.view.ViewUserInterface;

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
    
    private ModelImage [] currentDef;
    
    private ModelImage EPI4dto3dVolume;
    
    private int XN, YN, ZN, TN;

    float[] dimRes;
    
    private int[] epiExtents;
    
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
        if (command.equals("TEST")){ 
            if(pipeline.DWINewB0Image !=null){
                //EPIAlgorithm();
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
        
        mainEPIPanel = new JPanel();
        mainEPIPanel.setLayout(new GridBagLayout());
        

        // gbc.anchor = GridBagConstraints.NORTHWEST;
        gbc.gridx = 0;
        gbc.gridy = 0;
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
    
    private void EPIAlgorithm(){
        XN = pipeline.DWINewB0Image.getExtents()[0];
        YN = pipeline.DWINewB0Image.getExtents()[1];
        ZN = pipeline.DWINewB0Image.getExtents()[2];
        TN = pipeline.DWINewB0Image.getExtents()[3];
        dimRes = pipeline.DWINewB0Image.getResolutions(0);
        int sliceSize = XN * YN;
        int length = TN * sliceSize * ZN;

        epiExtents = new int[3];
        epiExtents[0] = pipeline.DWINewB0Image.getExtents()[0];
        epiExtents[1] = pipeline.DWINewB0Image.getExtents()[1];
        epiExtents[2] = pipeline.DWINewB0Image.getExtents()[2];
        
        EPI4dto3dArray = new ModelImage[TN];
        
        currentDef = new ModelImage[3];
        
        float[] imageBuffer = new float[sliceSize*ZN];
        
        for (int t = 0; t < TN-1; t++){
            System.out.println("workingc: " +t);
            EPI4dto3dArray[t] = new ModelImage(pipeline.DWINewB0Image.getType(), epiExtents, "EPI4dto3dArray");
                try {
                    System.out.println("t*sliceSize*ZN: " +t*sliceSize*ZN);
                    pipeline.DWINewB0Image.exportData(t*sliceSize*ZN, sliceSize*ZN);
                    EPI4dto3dArray[t].importData(0, imageBuffer, false);
                    
                }catch (IOException error) {
                    Preferences.debug("! ERROR: " + "EPI4dto3dArray" + "\n", Preferences.DEBUG_ALGORITHM);;
                    return;
                }
               /* if (pipeline.DWINewB0Image.isDicomImage() != true ){
                    FileInfoBase fileInfoBuffer; // buffer of any old type
                    fileInfoBuffer = (FileInfoBase) pipeline.DWINewB0Image.getFileInfo(t).clone();
                    fileInfoBuffer.setExtents(pipeline.DWINewB0Image.getExtents());
                    fileInfoBuffer.setResolutions(pipeline.DWINewB0Image.getFileInfo(0).getResolutions());
                    EPI4dto3dArray[t].setFileInfo(fileInfoBuffer, t);
                }*/
           
        }
        
    Matrix m;
    TransMatrix matarray;
    Matrix b0toStruct;
    b0toStruct = new Matrix(4,4);
    
    
    for (int i = 0; i < pipeline.arrayTransMatrix.length-1; i++){
        //File currentFile = DWtoB0Trans.get(i);
        //apply transformations or deformations one by one
        //currentDef = new ImageDataFloat[chN];                       
            m = new Matrix(4,4);
                for(int j=0; j<4; j++){
                    for(int k=0; k<4; k++){                            
                        m.set(j, k, pipeline.arrayTransMatrix[i].Get(j, k));
                        //System.out.println("matrixm" +m.print(4, 6));
                        b0toStruct.set(j, k, pipeline.b0toStructMatrix.Get(j, k));                           
                        //System.out.println("b0toStruct" +b0toStruct);
                    }                   
                }
                System.out.println("transmatrix" +pipeline.arrayTransMatrix[i]);
                //m.print(4, 6);
                //System.out.println("b0toStructMatrix" +pipeline.b0toStructMatrix);
                //b0toStruct.print(4, 6);
                m = b0toStruct.times(m);
                m.print(4, 6);
                
                //currentDef=createDefFieldFromTransMatrix(m);

        }
    
    
    
    }
    
   /* public ModelImage[] applyDeformation(ModelImage[] currentDef, ModelImage[] epiDef){
        double[] currentVec = new double[3];
        double[] newVec = new double[3];
        
        ModelImage[] newDef = new ModelImage[3];
        for (int c = 0; c < 3; c++) 
            newDef[c] = (ModelImage) currentDef[c].clone();
        

        for(int i = 0; i < XN; i++)
            for(int j = 0; j < YN; j++)
                for(int k = 0; k < ZN; k++){
                    
                    for(int c = 0; c < chN; c++) newVec[c] = epiDef[c].getDouble(i, j, k); 
                    
                    //get current deformation at where the new deformation is pointing
                    for(int c = 0; c < chN; c++) currentVec[c] = RegistrationUtilities.Interpolation(currentDef[c], XN, YN, ZN, 
                            newVec[0]+i, newVec[1]+j, newVec[2]+k, InterpolationType.TRILINEAR); 
                    
                    for(int c = 0; c < chN; c++) newDef[c].set(i,j,k, newVec[c]+currentVec[c]);
                    
                }
        return newDef;          
        
    }*/
    
    public ModelImage[] createDefFieldFromTransMatrix(Matrix newTrans){
        double[] currentVec = new double[3];
        double[] newVec = new double[3];
        ModelImage[] newDef = new ModelImage[3];
        
        for(int ch = 0; ch < 3; ch++){ 
            newDef[ch] = new ModelImage(pipeline.DWINewB0Image.getType(), epiExtents, "newDefArray");
        }
            for(int i = 0; i < XN; i++){
                for(int j = 0; j < YN; j++){
                    for(int k = 0; k < ZN; k++){                    
                    newVec[0] = i*dimRes[0];
                    newVec[1] = j*dimRes[1];
                    newVec[2] = k*dimRes[2];
                                        
                    Matrix v = new Matrix(new double[]{newVec[0],newVec[1],newVec[2],1},4);
                    Matrix vp = newTrans.solve(v);
                    
                    for(int ch = 0; ch < 3; ch++) {
                        newVec[ch] = vp.get(ch,0) - newVec[ch];
                        newDef[ch].set(i,j,k, newVec[ch]/dimRes[ch]);
                    }
                    
                    //get current deformation at where the new deformation is pointing
                    //for(int c = 0; c < chN; c++) currentVec[c] = RegistrationUtilities.Interpolation(currentDef[c], XN, YN, ZN, 
                    //      newVec[0]+i, newVec[1]+j, newVec[2]+k, InterpolationType.TRILINEAR); 

                    //for(int c = 0; c < chN; c++) newDef[c].set(i,j,k, newVec[c]+currentVec[c]);
                    
                    
                    }
                }
            
        }
        
        return newDef;
        
    }

    @Override
    public void itemStateChanged(ItemEvent e) {
        // TODO Auto-generated method stub
        
    }

}
