import gov.nih.mipav.plugins.*;
import gov.nih.mipav.model.file.FileIO;
import gov.nih.mipav.model.file.FileVOI;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.VOI;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.ViewImageFileFilter;
import gov.nih.mipav.view.ViewUserInterface;
import gov.nih.mipav.view.components.PanelManager;
import gov.nih.mipav.view.components.WidgetFactory;
import gov.nih.mipav.view.dialogs.JDialogBase;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.WindowEvent;
import java.io.File;
import java.io.IOException;

import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JFileChooser;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTextField;
import javax.swing.border.LineBorder;


public class PlugInDialogWormStraightening extends JDialogStandalonePlugin
{
	private static final long serialVersionUID = 2020338731116362598L;

	private ModelImage wormImage;
	
	private JTextField wormImageTextField, pointsFileTextField;
	private JTextField tailDiameterTextField, headDiameterTextField, maxDiameterTextField, stepSizeTextField, pixelSizeTextField;
	private JCheckBox fillInMissingData, displayOriginalImage, displayMaskImage;
	
	public PlugInDialogWormStraightening() {}
	
	public PlugInDialogWormStraightening(boolean modal)
	{
		init();
        setVisible(true);
	}
	
	public void actionPerformed(ActionEvent e)
	{
		String command = e.getActionCommand();
		Object source = e.getSource();
		
		if (command.equalsIgnoreCase("wormImageBrowse")) {
            JFileChooser chooser = new JFileChooser();   
            if (ViewUserInterface.getReference().getDefaultDirectory() != null) {
                chooser.setCurrentDirectory(new File(ViewUserInterface.getReference().getDefaultDirectory()));
            } else {
                chooser.setCurrentDirectory(new File(System.getProperties().getProperty("user.dir")));
            }
            chooser.setDialogTitle("Choose image");
            chooser.setFileSelectionMode(JFileChooser.FILES_ONLY);
            int returnValue = chooser.showOpenDialog(this);
            if (returnValue == JFileChooser.APPROVE_OPTION) {
                FileIO fileIO = new FileIO();
                if(wormImage != null) {
                	wormImage.disposeLocal();
                	wormImage = null;
                }
                wormImage = fileIO.readImage(chooser.getSelectedFile().getName(), chooser.getCurrentDirectory() + File.separator, false, null);              
                wormImageTextField.setText(chooser.getSelectedFile().getAbsolutePath());
                ViewUserInterface.getReference().setDefaultDirectory(chooser.getCurrentDirectory().toString() );
            }
        }else if(command.equals("pointsFileBrowse")) {
        	JFileChooser chooser = new JFileChooser();
            if (ViewUserInterface.getReference().getDefaultDirectory() != null) {
                chooser.setCurrentDirectory(new File(ViewUserInterface.getReference().getDefaultDirectory()));
            } else {
                chooser.setCurrentDirectory(new File(System.getProperties().getProperty("user.dir")));
            }
            chooser.setDialogTitle("Choose points file");
            chooser.setFileSelectionMode(JFileChooser.FILES_ONLY);
            chooser.addChoosableFileFilter(new ViewImageFileFilter(new String[] { ".xml", ".voi" }));
            int returnValue = chooser.showOpenDialog(this);
            if (returnValue == JFileChooser.APPROVE_OPTION) {
                String fileName = chooser.getSelectedFile().getName();
                String directory = String.valueOf(chooser.getCurrentDirectory()) + File.separatorChar;
                try {
                	FileVOI fileVOI = new FileVOI(fileName, directory, wormImage);
                	
                	VOI[] voi = fileVOI.readVOI(false);
                	
                	for (int i = 0; i < voi.length; i++) {
                		wormImage.registerVOI(voi[i]);
                    }

                	wormImage.notifyImageDisplayListeners();
                	
                	pointsFileTextField.setText(chooser.getSelectedFile().getAbsolutePath());

                    ViewUserInterface.getReference().setDefaultDirectory(chooser.getCurrentDirectory().toString() );
                	
                }catch(IOException ex) {
                	return;
                }
            }
        } else if (source == OKButton) {
        	setVisible(false);
        	callAlgorithm();
        	disposeLocal();
        } else if (source == cancelButton) {
        	this.windowClosing(new WindowEvent(this, WindowEvent.WINDOW_CLOSING));
            
        	setVisible(false);
        	disposeLocal();        	
        } else {
            super.actionPerformed(e);
        }

	}
		
	public void disposeLocal()
	{
		wormImage = null;
	}
	
	
	public void init()
	{
		setTitle("Worm Straightening");	
        getContentPane().setLayout(new BorderLayout());

        final PanelManager paramPanelManager = new PanelManager();
        paramPanelManager.add( new JLabel("Worm Image:") );
		wormImageTextField = new JTextField(55);
		wormImageTextField.setEditable(false);
		wormImageTextField.setBackground(Color.white);
		wormImageTextField.setBorder(new LineBorder(Color.black));
        paramPanelManager.add( wormImageTextField );
		JButton wormImageBrowseButton = new JButton("Browse");
		wormImageBrowseButton.addActionListener(this);
		wormImageBrowseButton.setActionCommand("wormImageBrowse");	
        paramPanelManager.add( wormImageBrowseButton );
        
        paramPanelManager.addOnNextLine(new JLabel("Points File:"));
		pointsFileTextField = new JTextField(55);
		pointsFileTextField.setEditable(false);
		pointsFileTextField.setBackground(Color.white);
		pointsFileTextField.setBorder(new LineBorder(Color.black));
        paramPanelManager.add( pointsFileTextField );
		JButton pointsFileBrowseButton = new JButton("Browse");
		pointsFileBrowseButton.addActionListener(this);
		pointsFileBrowseButton.setActionCommand("pointsFileBrowse");
        paramPanelManager.add( pointsFileBrowseButton );
        
        paramPanelManager.addOnNextLine(new JLabel("Path step-size:"));
		stepSizeTextField = new JTextField( "2.5", 10 );
		stepSizeTextField.setBackground(Color.white);
		stepSizeTextField.setBorder(new LineBorder(Color.black));
        paramPanelManager.add( stepSizeTextField );
        
        paramPanelManager.addOnNextLine(new JLabel("Estimate head diameter:"));
		headDiameterTextField = new JTextField( "12", 10 );
		headDiameterTextField.setBackground(Color.white);
		headDiameterTextField.setBorder(new LineBorder(Color.black));
        paramPanelManager.add( headDiameterTextField );
        
        paramPanelManager.addOnNextLine(new JLabel("Estimate tail diameter:"));
		tailDiameterTextField = new JTextField( "20", 10 );
		tailDiameterTextField.setBackground(Color.white);
		tailDiameterTextField.setBorder(new LineBorder(Color.black));
        paramPanelManager.add( tailDiameterTextField );
        
        paramPanelManager.addOnNextLine(new JLabel("Estimate maximum diameter:"));
		maxDiameterTextField = new JTextField( "30", 10 );
		maxDiameterTextField.setBackground(Color.white);
		maxDiameterTextField.setBorder(new LineBorder(Color.black));
        paramPanelManager.add( maxDiameterTextField );
        
        paramPanelManager.addOnNextLine(new JLabel("Pixel resolution:"));
        pixelSizeTextField = new JTextField( "0.1625", 10 );
        pixelSizeTextField.setBackground(Color.white);
        pixelSizeTextField.setBorder(new LineBorder(Color.black));
        paramPanelManager.add( pixelSizeTextField );
        paramPanelManager.add( new JLabel("um") );
        
        fillInMissingData = new JCheckBox("fill in missing data", true );
        paramPanelManager.addOnNextLine(fillInMissingData);
        
        displayOriginalImage = new JCheckBox("Display Original Image", false );
        paramPanelManager.addOnNextLine(displayOriginalImage);
        
        displayMaskImage = new JCheckBox("Display Mask Image", false );
        paramPanelManager.addOnNextLine(displayMaskImage);
        

        getContentPane().add(paramPanelManager.getPanel(), BorderLayout.CENTER);
        getContentPane().add(buildButtons(), BorderLayout.SOUTH);
        pack();
        setResizable(false);

        System.gc();
	}
		
	
	protected void callAlgorithm()
	{
		if ( wormImage == null )
		{
			MipavUtil.displayError( "Must specify initial image" );
		}
		try {
			float stepSize = Float.parseFloat(stepSizeTextField.getText().trim());
			float headSize = Float.parseFloat(headDiameterTextField.getText().trim());
			float tailSize = Float.parseFloat(tailDiameterTextField.getText().trim());
			float maxSize = Float.parseFloat(maxDiameterTextField.getText().trim());
			float pixelSize = Float.parseFloat(pixelSizeTextField.getText().trim());
			PlugInAlgorithmWormStraighteningAutomatic alg = new PlugInAlgorithmWormStraighteningAutomatic(wormImage, stepSize);
			alg.setDiameter(headSize/pixelSize, tailSize/pixelSize, maxSize/pixelSize);
			alg.setFill(fillInMissingData.isSelected());
			alg.setOutput(displayOriginalImage.isSelected(), displayMaskImage.isSelected());

			if (isRunInSeparateThread()) {

				// Start the thread as a low priority because we wish to still
				// have user interface work fast.
				if (alg.startMethod(Thread.MIN_PRIORITY) == false) {
					MipavUtil.displayError("A thread is already running on this object");
				}
			} else {
				alg.run();
			}
		}
		catch (NumberFormatException e)
		{
			MipavUtil.displayError("Please enter a valid step-size" );
		}
	}

}
