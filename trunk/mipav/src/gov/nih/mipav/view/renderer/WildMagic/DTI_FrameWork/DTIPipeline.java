package gov.nih.mipav.view.renderer.WildMagic.DTI_FrameWork;


import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.algorithms.AlgorithmInterface;
import gov.nih.mipav.model.file.DTIParameters;
import gov.nih.mipav.model.file.FileIO;
import gov.nih.mipav.model.scripting.ParserException;
import gov.nih.mipav.model.structures.ModelImage;

import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.ViewJFrameImage;
import gov.nih.mipav.view.ViewUserInterface;
import gov.nih.mipav.view.dialogs.ActionDiscovery;
import gov.nih.mipav.view.dialogs.JDialogBase;
import gov.nih.mipav.view.dialogs.JDialogScriptableBase;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.Frame;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTabbedPane;
import javax.swing.ScrollPaneConstants;


public class DTIPipeline extends JDialogBase implements AlgorithmInterface, ActionListener {
    /** main panel * */
    public JPanel mainPanel;
    
    /** DOCUMENT ME! */
   protected JButton nextButton; 
   
   public ModelImage DWIImage;
   
   public ModelImage T2Image;
    
    /** DOCUMENT ME! */
    private JButton goBackButton;
    
    private JPanelDTIImportData importData;
    
    //private JPanelDTIFiberTrack fiberTrack;
    
    private JPanelDTIFiberTracking fiberTrack;
    
    private DTIParameters dtiparams;
    
    private JPanelDTIVisualization visualization;
    
    private JPanelDTIRegistrationEddyCurrent35D eddyCurReg;
    
    private JPanelT2Load t2load;

    /** DOCUMENT ME! */
    private ModelImage image; // source image

    /** DOCUMENT ME! */
    private ViewUserInterface userInterface;

    /** DOCUMENT ME! */
    protected JTabbedPane tabbedPane;
    
    public ViewJFrameImage DWIframe;
    
    public ViewJFrameImage T2frame;

    
   

    // ~ Constructors
    // ---------------------------------------------------------------------------------------------------

    public DTIPipeline() {
        super(ViewUserInterface.getReference().getMainFrame(), false);
        
        //Testing
        /*String fileDir = "C:\\Users\\tyrieek\\Desktop\\PARRECTest\\Landman_4.2.REC";
        //String fileName = "DTI_2MM_FREE_39VOL_SET2_10.dcm";
        FileIO fileIO = new FileIO();
        image = fileIO.readImage(fileDir);
        new ViewJFrameImage(image);*/
        
        init();
    }
    


    /**
     * init
     */

    public void init() {
        System.out.println("working");
        setForeground(Color.black);
        setTitle("DTI Pipeline");

        mainPanel = new JPanel();
        mainPanel.setLayout(new BorderLayout());
        //mainPanel.setLayout(new BoxLayout(mainPanel, BoxLayout.Y_AXIS));

        tabbedPane = new JTabbedPane();

        setTitle("DTI Pipeline");
        tabbedPane.addTab("Import Data", null, buildImportDataPanel());
        tabbedPane.addTab("T2 Image Registration", null, buildT2Panel());
        tabbedPane.addTab("Motion Correction/Eddy Current", null, buildRegEddyCurPanel());
        tabbedPane.addTab("EPI Distortion Correction", null, buildEPIPanel());
        tabbedPane.addTab("Tensor Estimation", null, buildTensorPanel());
        tabbedPane.addTab("Fiber Tracking/ Statistics", null, buildFiberTrackingPanel());
        tabbedPane.addTab("Visualization", null, buildVisuzalizationPanel());
       
        mainPanel.add(tabbedPane);

        final JPanel NextGoBackPanel = new JPanel();
        goBackButton = new JButton("Back");
        goBackButton.addActionListener(this);
        goBackButton.setActionCommand("back");
        goBackButton.setEnabled(false);
        NextGoBackPanel.add(goBackButton, BorderLayout.WEST);

        nextButton = new JButton("Next");
        nextButton.addActionListener(this);
        nextButton.setActionCommand("next");
        nextButton.setEnabled(false);
        NextGoBackPanel.add(nextButton, BorderLayout.EAST);
        
       
        mainPanel.setPreferredSize(new Dimension(1000, 750));
        getContentPane().add(mainPanel, BorderLayout.CENTER);
        getContentPane().add(NextGoBackPanel, BorderLayout.SOUTH);
        pack();
        setVisible(true);

    }

    // ~ Methods
    // --------------------------------------------------------------------------------------------------------

    /**
     * action performed
     */
    public void actionPerformed(final ActionEvent event) {
        final String command = event.getActionCommand();
        System.out.println("actionperformed");

        if (command.equals("next1")){
            DWIImage = importData.m_kDWIImage;
            DWIframe = importData.frame;
            dtiparams = DWIImage.getDTIParameters();

            if (dtiparams.getbValues() != null && dtiparams.getGradients() != null){
                System.out.println("bothnotnull");
                tabbedPane.setSelectedIndex(1);
                nextButton.setEnabled(false);
                goBackButton.setEnabled(true);
                goBackButton.setActionCommand("back1");
            }
                else{
                    MipavUtil.displayError("Please load B-values and Gradients");
                }
            
           if (importData.useT2CheckBox.isSelected()){
               if (importData.m_kT2Image !=null){
                   System.out.println("t2imagenotnull");
                   T2Image = importData.m_kT2Image;
                   T2frame = importData.t2frame;
                   eddyCurReg.epiCheckBox.setEnabled(true);
               }
               else{
                   MipavUtil.displayError("Please load T2 image"); 
                   tabbedPane.setSelectedIndex(0);
               }
           }
           
           if (T2Image != null) {
               t2load.matrixComboBox.addItem(T2Image.getImageDirectory());   
           }


            
            
            
        }
        
        else if (command.equals("back1")){
            System.out.println("backworking");
            tabbedPane.setSelectedIndex(0);
            nextButton.setEnabled(true);
            goBackButton.setEnabled(false);
        }
        
   
        

    }
    
    private JScrollPane buildImportDataPanel() {
        
        importData = new JPanelDTIImportData(this);
        System.out.println("working1");
        return importData.scrollPane;
    }
    
    private JPanel buildT2Panel() {
        System.out.println("working3");
        t2load = new JPanelT2Load(this);
        final JPanel wholePanel = new JPanel();
        //wholePanel.setLayout(new BoxLayout(wholePanel, BoxLayout.Y_AXIS));
        //final JScrollPane scrollPane = new JScrollPane(wholePanel, ScrollPaneConstants.VERTICAL_SCROLLBAR_AS_NEEDED,
                //ScrollPaneConstants.HORIZONTAL_SCROLLBAR_NEVER);
        //wholePanel.setPreferredSize(new Dimension(595, 200));

        return t2load.mainT2Panel;

    }

    private JPanel buildRegEddyCurPanel() {

        System.out.println("working2");
        eddyCurReg = new JPanelDTIRegistrationEddyCurrent35D(this);

        //wholePanel.setLayout(new BoxLayout(wholePanel, BoxLayout.Y_AXIS));
        //final JScrollPane scrollPane = new JScrollPane(wholePanel, ScrollPaneConstants.VERTICAL_SCROLLBAR_AS_NEEDED,
                //ScrollPaneConstants.HORIZONTAL_SCROLLBAR_NEVER);
        //wholePanel.setPreferredSize(new Dimension(595, 200));

        return eddyCurReg.mainRegPanel ;
    }

    private JPanel buildEPIPanel() {
        System.out.println("working3");
        final JPanel wholePanel = new JPanel();
        //wholePanel.setLayout(new BoxLayout(wholePanel, BoxLayout.Y_AXIS));
        //final JScrollPane scrollPane = new JScrollPane(wholePanel, ScrollPaneConstants.VERTICAL_SCROLLBAR_AS_NEEDED,
                //ScrollPaneConstants.HORIZONTAL_SCROLLBAR_NEVER);
        //wholePanel.setPreferredSize(new Dimension(595, 200));

        return wholePanel;

    }

    private JPanel buildTensorPanel() {
        System.out.println("working4");
        final JPanel wholePanel = new JPanel();
        //wholePanel.setLayout(new BoxLayout(wholePanel, BoxLayout.Y_AXIS));
        //final JScrollPane scrollPane = new JScrollPane(wholePanel, ScrollPaneConstants.VERTICAL_SCROLLBAR_AS_NEEDED,
                //ScrollPaneConstants.HORIZONTAL_SCROLLBAR_NEVER);
        //wholePanel.setPreferredSize(new Dimension(595, 200));

        return wholePanel;

    }

    private JPanelDTIFiberTracking buildFiberTrackingPanel() {
        System.out.println("working5");
        fiberTrack = new JPanelDTIFiberTracking(image);
        //final JPanel wholePanel = new JPanel();
        //wholePanel.setLayout(new BoxLayout(wholePanel, BoxLayout.Y_AXIS));
        //final JScrollPane scrollPane = new JScrollPane(wholePanel, ScrollPaneConstants.VERTICAL_SCROLLBAR_AS_NEEDED,
                //ScrollPaneConstants.HORIZONTAL_SCROLLBAR_NEVER);
        //wholePanel.setPreferredSize(new Dimension(595, 200));

        return fiberTrack;

    }

    private JPanel buildVisuzalizationPanel() {
        System.out.println("working6");
        
        visualization = new JPanelDTIVisualization(image); 
        //final JPanel wholePanel = new JPanel();
        //wholePanel.setLayout(new BoxLayout(wholePanel, BoxLayout.Y_AXIS));
        //final JScrollPane scrollPane = new JScrollPane(wholePanel, ScrollPaneConstants.VERTICAL_SCROLLBAR_AS_NEEDED,
                //ScrollPaneConstants.HORIZONTAL_SCROLLBAR_NEVER);
        //wholePanel.setPreferredSize(new Dimension(595, 200));

        return visualization;
    }



    public void algorithmPerformed(final AlgorithmBase algorithm) {

    }

}
