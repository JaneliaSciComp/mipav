import gov.nih.mipav.model.file.FileIO;
import gov.nih.mipav.model.file.FileVOI;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.VOI;
import gov.nih.mipav.plugins.JDialogStandalonePlugin;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.Preferences;
import gov.nih.mipav.view.ViewImageFileFilter;
import gov.nih.mipav.view.ViewUserInterface;
import gov.nih.mipav.view.components.PanelManager;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.event.ActionEvent;
import java.awt.event.WindowEvent;
import java.io.File;
import java.io.IOException;

import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JFileChooser;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTabbedPane;
import javax.swing.JTextField;
import javax.swing.border.LineBorder;


/**
 * Dialog sets up inputs for the worm straightening algorithm.
 * 
 * There are three options, each on a different tab.
 * 1. Automatic Path generation. Attempts to automatically detect the digestive tract inside the worm and
 *    generate a VOI path along the tract, starting at a characteristic 'tail' and ending at the worm 'head'.
 *    The user specifies the input image, length of the worm, head, tail and maximum diameter and the scale of the voxels.
 * 2. Worm Straightening. Applies a VOI path which is down the center-line of the worm starting at the 'tail' and
 *    ending at the 'head' of the worm. The VOI points may be generated in the automatically or by placing them by hand using
 *    the VOI tool.  The user specifies the input image, VOI points specifing the path (file), the head, tail, and maximum worm 
 *    diameter, the scale of the voxels, a step-size for sampling the original image, and then the option to fill-in missing data
 *    for areas where the sampling planes overlap, for example the inside of a fold.  The user can also choose to save
 *    the transformations and paramaters used to straighten the worm so the same transform can be applied to a different image
 *    or to different image channels.
 * 3. Applying a saved transform to a new image.  The user specifies the input image and a save transform file generated in the
 *    worm-straightening pass. The user also has the option to fill-in missing data on the straightened image.
 *
 */
public class PlugInDialogWormStraightening extends JDialogStandalonePlugin
{
	private static final long serialVersionUID = 3248364538723357418L;


	/**
	 *  User-interface components for the automatic path interface.
	 */
	private class LatticePathUI
	{		
		private JTextField wormImageTextField;
		private JTextField latticeFileField, pixelSizeTextField;
	}

	/**
	 *  User-interface components for the straightening interface.
	 */
	private class StraightenUI
	{		
		private JTextField wormImageTextField, pointsFileTextField;
		private JTextField tailDiameterTextField, headDiameterTextField, maxDiameterTextField, stepSizeTextField, pixelSizeTextField;
		private JCheckBox fillInMissingData, displayOriginalImage, displayMaskImage, saveTransform;

		private JButton pointsFileBrowseButton;
	}

	/**
	 *  User-interface components for loading and applying a transform.
	 */
	private class TransformUI
	{		
		private JTextField wormImageTextField, transformFileTextField;
		private JCheckBox fillInMissingData;
	}
	
	/** Input image. */
	private ModelImage wormImage;
	/** tabbed pane interface */
	private JTabbedPane tabbedPane = new JTabbedPane();
	

    private LatticePathUI latticeInterface;
	private StraightenUI straightenInterface;
	private TransformUI transformInterface;
	
	public PlugInDialogWormStraightening() {}

	public PlugInDialogWormStraightening( boolean modal )
	{
		init();
        setVisible(true);
	}
	
	public void actionPerformed(ActionEvent e)
	{
		String command = e.getActionCommand();
		Object source = e.getSource();
		
		if ( command.equalsIgnoreCase("wormImageBrowse") )
		{
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

                if ( tabbedPane.getSelectedIndex() == 0 )
                {
                	latticeInterface.wormImageTextField.setText(chooser.getSelectedFile().getAbsolutePath());
                	ViewUserInterface.getReference().setDefaultDirectory(chooser.getCurrentDirectory().toString() );
                }
                else if ( tabbedPane.getSelectedIndex() == 1 )
                {
                	straightenInterface.wormImageTextField.setText(chooser.getSelectedFile().getAbsolutePath());
                	ViewUserInterface.getReference().setDefaultDirectory(chooser.getCurrentDirectory().toString() );
                	straightenInterface.pointsFileBrowseButton.setEnabled(true);
                	straightenInterface.pointsFileTextField.setEnabled(true);
    			}
                else if ( tabbedPane.getSelectedIndex() == 2 )
                {
                	transformInterface.wormImageTextField.setText(chooser.getSelectedFile().getAbsolutePath());
                	ViewUserInterface.getReference().setDefaultDirectory(chooser.getCurrentDirectory().toString() );
    			}
            }
        } 
		else if ( command.equals("latticeBrowse") )
		{
            // get the voi directory
            String fileName = null;
            String directory = null;
            String voiDir = null;

            final JFileChooser chooser = new JFileChooser();

            if (ViewUserInterface.getReference().getDefaultDirectory() != null) {
                chooser.setCurrentDirectory(new File(ViewUserInterface.getReference().getDefaultDirectory()));
            } else {
                chooser.setCurrentDirectory(new File(System.getProperties().getProperty("user.dir")));
            }

            chooser.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);

            final int returnVal = chooser.showOpenDialog(this);

            if (returnVal == JFileChooser.APPROVE_OPTION) {
                fileName = chooser.getSelectedFile().getName();
                directory = String.valueOf(chooser.getCurrentDirectory()) + File.separatorChar;
                Preferences.setProperty(Preferences.PREF_IMAGE_DIR, chooser.getCurrentDirectory().toString());
            }

            if (fileName != null) {
                voiDir = new String(directory + fileName + File.separator);
                latticeInterface.latticeFileField.setText(voiDir);
            }
		}
		else if ( command.equals("pointsFileBrowse" ) ) 
		{
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

                	straightenInterface.pointsFileTextField.setText(chooser.getSelectedFile().getAbsolutePath());

                    ViewUserInterface.getReference().setDefaultDirectory(chooser.getCurrentDirectory().toString() );
                	
                }catch(IOException ex) {
                	return;
                }
            }
        } 
        else if ( command.equals("transformFileBrowse" ) )
        {
        	JFileChooser chooser = new JFileChooser();
        	if (ViewUserInterface.getReference().getDefaultDirectory() != null) {
        		chooser.setCurrentDirectory(new File(ViewUserInterface.getReference().getDefaultDirectory()));
        	} else {
        		chooser.setCurrentDirectory(new File(System.getProperties().getProperty("user.dir")));
        	}
        	chooser.setDialogTitle("Choose points file");
        	chooser.setFileSelectionMode(JFileChooser.FILES_ONLY);
        	int returnValue = chooser.showOpenDialog(this);
        	if (returnValue == JFileChooser.APPROVE_OPTION) {
            	transformInterface.transformFileTextField.setText(chooser.getSelectedFile().getAbsolutePath());
        	}
        }
        else if (source == OKButton)
        {
        	setVisible(false);
        	callAlgorithm();
        	disposeLocal();
        } 
        else if (source == cancelButton)
        {			
        	this.windowClosing(new WindowEvent(this, WindowEvent.WINDOW_CLOSING));
            
        	setVisible(false);
        	disposeLocal();        	
        } 
        else
        {
            super.actionPerformed(e);
        }

	}
	

	public void disposeLocal()
	{
		wormImage = null;
	}
	
	protected void callAlgorithm()
	{
		if ( wormImage == null )
		{
			MipavUtil.displayError( "Must specify initial image" );
		}
		try {
			if ( tabbedPane.getSelectedIndex() == 0 )
			{				
				float pixelSize = Float.parseFloat(latticeInterface.pixelSizeTextField.getText().trim());
				PlugInAlgorithmWormStraighteningAutomatic alg = new PlugInAlgorithmWormStraighteningAutomatic(wormImage);
				alg.setLatticeFile(latticeInterface.latticeFileField.getText());

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
			else if ( tabbedPane.getSelectedIndex() == 1 )
			{
				float stepSize = Float.parseFloat(straightenInterface.stepSizeTextField.getText().trim());
				float headSize = Float.parseFloat(straightenInterface.headDiameterTextField.getText().trim());
				float tailSize = Float.parseFloat(straightenInterface.tailDiameterTextField.getText().trim());
				float maxSize = Float.parseFloat(straightenInterface.maxDiameterTextField.getText().trim());
				float pixelSize = Float.parseFloat(straightenInterface.pixelSizeTextField.getText().trim());
				PlugInAlgorithmWormStraighteningAutomatic alg = new PlugInAlgorithmWormStraighteningAutomatic(wormImage, stepSize);
				alg.setDiameter(headSize/pixelSize, tailSize/pixelSize, maxSize/pixelSize);
				alg.setFill(straightenInterface.fillInMissingData.isSelected());
				alg.setOutput(straightenInterface.displayOriginalImage.isSelected(), straightenInterface.displayMaskImage.isSelected());
				alg.setSaveTransform( straightenInterface.saveTransform.isSelected() );

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
			else if ( tabbedPane.getSelectedIndex() == 2 )
			{
				PlugInAlgorithmWormStraighteningAutomatic alg = new PlugInAlgorithmWormStraighteningAutomatic(wormImage);
				alg.setTransformFile(transformInterface.transformFileTextField.getText());
				alg.setFill(transformInterface.fillInMissingData.isSelected());

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
		}
		catch (NumberFormatException e)
		{
			MipavUtil.displayError("Please enter a valid step-size" );
		}
	}
	
	private void init()
	{
		setTitle("Worm Straightening version 1.1");	
        getContentPane().setLayout(new BorderLayout());
        
        tabbedPane.add( "Automatic Path", initLatticePanel() );      
        tabbedPane.add( "Straighten", initStraightenPanel() );
        tabbedPane.add( "Apply Transform", initTransformPanel() );

        getContentPane().add(tabbedPane, BorderLayout.CENTER);
        getContentPane().add(buildButtons(), BorderLayout.SOUTH);
        pack();
        setResizable(false);

        tabbedPane.setSelectedIndex(1);
        
        System.gc();
	}
	
	private JPanel initLatticePanel()
	{
		latticeInterface = new LatticePathUI();
		
        final PanelManager paramPanelManager = new PanelManager();
        
        paramPanelManager.add( new JLabel("Worm Image:") );
        latticeInterface.wormImageTextField = new JTextField(55);
        latticeInterface.wormImageTextField.setEditable(false);
        latticeInterface.wormImageTextField.setBackground(Color.white);
        latticeInterface.wormImageTextField.setBorder(new LineBorder(Color.black));
        paramPanelManager.add( latticeInterface.wormImageTextField );
		JButton wormImageBrowseButton = new JButton("Browse");
		wormImageBrowseButton.addActionListener(this);
		wormImageBrowseButton.setActionCommand("wormImageBrowse");	
        paramPanelManager.add( wormImageBrowseButton );
        

        paramPanelManager.addOnNextLine( new JLabel( "Lattice VOI directory: " ) );
        latticeInterface.latticeFileField = new JTextField(55);
        latticeInterface.latticeFileField.setEditable(false);
        latticeInterface.latticeFileField.setBackground(Color.white);
        latticeInterface.latticeFileField.setBorder(new LineBorder(Color.black));
        paramPanelManager.add( latticeInterface.latticeFileField );
		JButton latticeBrowseButton = new JButton("Browse");
		latticeBrowseButton.addActionListener(this);
		latticeBrowseButton.setActionCommand("latticeBrowse");	
        paramPanelManager.add( latticeBrowseButton );
                
        paramPanelManager.addOnNextLine(new JLabel("Pixel resolution:"));
        latticeInterface.pixelSizeTextField = new JTextField( "0.1625", 10 );
        latticeInterface.pixelSizeTextField.setBackground(Color.white);
        latticeInterface.pixelSizeTextField.setBorder(new LineBorder(Color.black));
        paramPanelManager.add( latticeInterface.pixelSizeTextField );
        paramPanelManager.add( new JLabel("um") );
        

        return paramPanelManager.getPanel();
	}
	
	
	private JPanel initStraightenPanel()
	{
		straightenInterface = new StraightenUI();
		
        final PanelManager paramPanelManager = new PanelManager();
        paramPanelManager.add( new JLabel("Worm Image:") );
        straightenInterface.wormImageTextField = new JTextField(55);
        straightenInterface.wormImageTextField.setEditable(false);
        straightenInterface.wormImageTextField.setBackground(Color.white);
        straightenInterface.wormImageTextField.setBorder(new LineBorder(Color.black));
        paramPanelManager.add( straightenInterface.wormImageTextField );
		JButton wormImageBrowseButton = new JButton("Browse");
		wormImageBrowseButton.addActionListener(this);
		wormImageBrowseButton.setActionCommand("wormImageBrowse");	
        paramPanelManager.add( wormImageBrowseButton );
        
        paramPanelManager.addOnNextLine(new JLabel("Points File:"));
        straightenInterface.pointsFileTextField = new JTextField(55);
        straightenInterface.pointsFileTextField.setEditable(false);
        straightenInterface.pointsFileTextField.setBackground(Color.white);
        straightenInterface.pointsFileTextField.setBorder(new LineBorder(Color.black));
        straightenInterface.pointsFileTextField.setEnabled(false);
        paramPanelManager.add( straightenInterface.pointsFileTextField );
        straightenInterface.pointsFileBrowseButton = new JButton("Browse");
        straightenInterface.pointsFileBrowseButton.addActionListener(this);
        straightenInterface.pointsFileBrowseButton.setActionCommand("pointsFileBrowse");
        paramPanelManager.add( straightenInterface.pointsFileBrowseButton );
        straightenInterface.pointsFileBrowseButton.setEnabled(false);

        
        paramPanelManager.addOnNextLine(new JLabel("Path step-size:"));
        straightenInterface.stepSizeTextField = new JTextField( "2.5", 10 );
        straightenInterface.stepSizeTextField.setBackground(Color.white);
        straightenInterface.stepSizeTextField.setBorder(new LineBorder(Color.black));
        paramPanelManager.add( straightenInterface.stepSizeTextField );
        
        paramPanelManager.addOnNextLine(new JLabel("Estimate head diameter:"));
        straightenInterface.headDiameterTextField = new JTextField( "12", 10 );
        straightenInterface.headDiameterTextField.setBackground(Color.white);
        straightenInterface.headDiameterTextField.setBorder(new LineBorder(Color.black));
        paramPanelManager.add( straightenInterface.headDiameterTextField );
        
        paramPanelManager.addOnNextLine(new JLabel("Estimate tail diameter:"));
        straightenInterface.tailDiameterTextField = new JTextField( "20", 10 );
        straightenInterface.tailDiameterTextField.setBackground(Color.white);
        straightenInterface.tailDiameterTextField.setBorder(new LineBorder(Color.black));
        paramPanelManager.add( straightenInterface.tailDiameterTextField );
        
        paramPanelManager.addOnNextLine(new JLabel("Estimate maximum diameter:"));
        straightenInterface.maxDiameterTextField = new JTextField( "30", 10 );
        straightenInterface.maxDiameterTextField.setBackground(Color.white);
        straightenInterface.maxDiameterTextField.setBorder(new LineBorder(Color.black));
        paramPanelManager.add( straightenInterface.maxDiameterTextField );
        
        paramPanelManager.addOnNextLine(new JLabel("Pixel resolution:"));
        straightenInterface.pixelSizeTextField = new JTextField( "0.1625", 10 );
        straightenInterface.pixelSizeTextField.setBackground(Color.white);
        straightenInterface.pixelSizeTextField.setBorder(new LineBorder(Color.black));
        paramPanelManager.add( straightenInterface.pixelSizeTextField );
        paramPanelManager.add( new JLabel("um") );
        
        straightenInterface.fillInMissingData = new JCheckBox("fill in missing data", true );
        paramPanelManager.addOnNextLine(straightenInterface.fillInMissingData);
        
        straightenInterface.displayOriginalImage = new JCheckBox("Display Original Image", false );
        paramPanelManager.addOnNextLine(straightenInterface.displayOriginalImage);
        
        straightenInterface.displayMaskImage = new JCheckBox("Display Mask Image", false );
        paramPanelManager.addOnNextLine(straightenInterface.displayMaskImage);
        
        straightenInterface.saveTransform = new JCheckBox("Save Transform", true );
        paramPanelManager.addOnNextLine(straightenInterface.saveTransform);
        
        return paramPanelManager.getPanel();
	}
		
	
	private JPanel initTransformPanel()
	{
		transformInterface = new TransformUI();
		
        final PanelManager paramPanelManager = new PanelManager();
        paramPanelManager.add( new JLabel("Worm Image:") );
        transformInterface.wormImageTextField = new JTextField(55);
        transformInterface.wormImageTextField.setEditable(false);
        transformInterface.wormImageTextField.setBackground(Color.white);
        transformInterface.wormImageTextField.setBorder(new LineBorder(Color.black));
        paramPanelManager.add( transformInterface.wormImageTextField );
		JButton wormImageBrowseButton = new JButton("Browse");
		wormImageBrowseButton.addActionListener(this);
		wormImageBrowseButton.setActionCommand("wormImageBrowse");	
        paramPanelManager.add( wormImageBrowseButton );
        
        paramPanelManager.addOnNextLine(new JLabel("Transform File:"));
        transformInterface.transformFileTextField = new JTextField(55);
        transformInterface.transformFileTextField.setEditable(false);
        transformInterface.transformFileTextField.setBackground(Color.white);
        transformInterface.transformFileTextField.setBorder(new LineBorder(Color.black));
        paramPanelManager.add( transformInterface.transformFileTextField );
        JButton browseButton = new JButton("Browse");
        browseButton.addActionListener(this);
        browseButton.setActionCommand("transformFileBrowse");
        paramPanelManager.add( browseButton );
               
        transformInterface.fillInMissingData = new JCheckBox("fill in missing data", true );
        paramPanelManager.addOnNextLine(transformInterface.fillInMissingData);
        
        return paramPanelManager.getPanel();
	}

}
