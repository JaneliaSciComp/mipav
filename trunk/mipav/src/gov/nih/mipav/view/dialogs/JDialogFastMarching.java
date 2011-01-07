package gov.nih.mipav.view.dialogs;

import gov.nih.mipav.model.algorithms.levelset.AlgorithmFastMarching;

import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.scripting.ParserException;
import gov.nih.mipav.model.scripting.parameters.ParameterExternalImage;
import gov.nih.mipav.model.scripting.parameters.ParameterFactory;
import gov.nih.mipav.model.scripting.parameters.ParameterBoolean;
import gov.nih.mipav.model.scripting.parameters.ParameterFloat;
import gov.nih.mipav.model.scripting.parameters.ParameterImage;
import gov.nih.mipav.model.scripting.parameters.ParameterInt;
import gov.nih.mipav.model.scripting.parameters.ParameterTable;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.awt.*;
import java.awt.event.*;

import javax.swing.*;


/**
 */
public class JDialogFastMarching extends JDialogScriptableBase implements ActionDiscovery, AlgorithmInterface {


    /** */
	private static final long serialVersionUID = -2307387171411648198L;

	/** Process each slice independently, or as a 3D image. */
    private JCheckBox image25DCheckbox;
    
    /** Source Image */
    private ModelImage image; // source image

    /** Number of Iterations */
    private JTextField textIters;

    /** Gradient Magnitude Scale factor */
    private JTextField textGMScale;

    /** Sigmoid Alpha parameter */
    private JTextField textSigmoidAlpha;

    /** Sigmoid Beta parameter */
    private JTextField textSigmoiBeta;

    /** Sigmoid Min parameter */
    private JTextField textSigmoidMin;

    /**Sigmoid Max parameter  */
    private JTextField textSigmoidMax;

    /** Max coarse iterations. */
    private JTextField textCoarseMax;
    
    /** Max Distance parameter */
    private JTextField textMaxDistance;

    /** Advection weight parameter */
    private JTextField textAdvectionWeight;

    /** Propagation weight parameter */
    private JTextField textPropagationWeight;

    /** Curvature weight parameter */
    private JTextField textCurvatureWeight;

    /** Laplacian weight parameter */
    private JTextField textLaplacianWeight;

    /** Maximum Evolution steps */
    private JTextField textMaxEvolution;
    
    private int m_iIters;
    private float m_fGMScale;
    private float m_fSAlpha;
    private float m_fSBeta;
    private float m_fSMin;
    private float m_fSMax;
    private int m_iCoarseMax;
    private float m_fMaxDistance;
    private float m_fAdvectionWeight;
    private float m_fPropagationWeight;
    private float m_fCurvatureWeight;
    private float m_fLaplacianWeight;
    private int m_iEvolveMax;
    private int m_iFilterType;
    private boolean m_bImage25D = false;

	/**
	 * Empty constructor needed for dynamic instantiation (used during scripting).
	 */
	public JDialogFastMarching() { }
	
    /**
     * Creates new dialog for finding the level set.
     *
     * @param  theParentFrame  Parent frame
     * @param  im              Source image
     */
    public JDialogFastMarching(Frame theParentFrame, ModelImage im, int iFilterType) {
        super(theParentFrame, false);
        image = im;
        m_iFilterType = iFilterType;

        ViewVOIVector VOIs = image.getVOIs();
        int nVOI = VOIs.size();

        if (nVOI == 0) {
            MipavUtil.displayError("Image must have a VOI for level set");
            return;
        }

        init();
    }

    /**
     * Closes dialog box when the OK button is pressed and calls the algorithm.
     *
     * @param  event  event that triggers function
     */
    public void actionPerformed(ActionEvent event) {
        Object source = event.getSource();
        String tmpStr;

        if (source == OKButton) {
        	m_iIters = 0;
        	tmpStr = textIters.getText();
        	if (testParameter(tmpStr, 1, 10000)) {
        		m_iIters = Integer.valueOf(tmpStr).intValue();
            } else {
            	textIters.requestFocus();
            	textIters.selectAll();
                return;
            }

        	m_fGMScale = 0;
        	tmpStr = textGMScale.getText();
        	if (testParameter(tmpStr, 0, Double.MAX_VALUE)) {
        		m_fGMScale = Float.valueOf(tmpStr).floatValue();
            } else {
            	textGMScale.requestFocus();
            	textGMScale.selectAll();
                return;
            }

        	m_fSAlpha = 0;
        	tmpStr = textSigmoidAlpha.getText();
        	m_fSAlpha = Float.valueOf(tmpStr).floatValue();
        	
        	m_fSBeta = 0;
        	tmpStr = textSigmoiBeta.getText();
        	m_fSBeta = Float.valueOf(tmpStr).floatValue();
        	
        	m_fSMin = 0;
        	tmpStr = textSigmoidMin.getText();
        	m_fSMin = Float.valueOf(tmpStr).floatValue();
        	
        	m_fSMax = 0;
        	tmpStr = textSigmoidMax.getText();
        	m_fSMax = Float.valueOf(tmpStr).floatValue();
        	
        	m_iCoarseMax = 0;
        	tmpStr = textCoarseMax.getText();
        	if (testParameter(tmpStr, 1, Integer.MAX_VALUE)) {
        		m_iCoarseMax = Integer.valueOf(tmpStr).intValue();
            } else {
            	textCoarseMax.requestFocus();
            	textCoarseMax.selectAll();
                return;
            }
        	
        	m_fMaxDistance = 0;
        	tmpStr = textMaxDistance.getText();
        	m_fMaxDistance = Float.valueOf(tmpStr).floatValue();
        	
        	m_fAdvectionWeight = 0;
        	tmpStr = textAdvectionWeight.getText();
        	m_fAdvectionWeight = Float.valueOf(tmpStr).floatValue();
        	
        	m_fPropagationWeight = 0;
        	tmpStr = textPropagationWeight.getText();
        	m_fPropagationWeight = Float.valueOf(tmpStr).floatValue();
        	
        	m_fCurvatureWeight = 0;
        	tmpStr = textCurvatureWeight.getText();
        	m_fCurvatureWeight = Float.valueOf(tmpStr).floatValue();
        	
        	m_fLaplacianWeight = 0;
        	tmpStr = textLaplacianWeight.getText();
        	m_fLaplacianWeight = Float.valueOf(tmpStr).floatValue();

        	m_iEvolveMax = 0;
        	tmpStr = textMaxEvolution.getText();
        	if (testParameter(tmpStr, 1, Integer.MAX_VALUE)) {
        		m_iEvolveMax = Integer.valueOf(tmpStr).intValue();
            } else {
            	textMaxEvolution.requestFocus();
            	textMaxEvolution.selectAll();
                return;
            }
        	if ( image25DCheckbox != null )
        	{
        		m_bImage25D = image25DCheckbox.isSelected();
        	}
        	callAlgorithm();

        } else if (source == cancelButton) {
            dispose();
        }
    }

    // ************************************************************************
    // ************************** Algorithm Events ****************************
    // ************************************************************************

    /**
     * This method is required if the AlgorithmPerformed interface is implemented. It is called by the algorithms when
     * it has completed or failed to to complete, so that the dialog can be display the result image and/or clean up.
     *
     * @param  algorithm  Algorithm that caused the event.
     */
    public void algorithmPerformed(AlgorithmBase algorithm) {

        if (algorithm instanceof AlgorithmFastMarching) {

            if (algorithm.isCompleted()) {
                image.notifyImageDisplayListeners(null, true);
            }
			insertScriptLine();
        }

        dispose();
    }

    /**
     * Sets up the GUI (panels, buttons, etc) and displays it on the screen.
     */
    private void init() {
        setForeground(Color.black);

        switch ( m_iFilterType )
        {
        case 0: setTitle("LevelSet Diffusion"); break;
        case 1: setTitle("LevelSet Geodesic Active Contour"); break;
        case 2: setTitle("LevelSet Threshold"); break;
        default: setTitle("LevelSet Diffusion"); break;
        }
        getContentPane().setLayout(new BorderLayout());

        JPanel mainPanel;
        mainPanel = new JPanel();
        mainPanel.setBorder(BorderFactory.createEmptyBorder(3, 3, 3, 3));
        mainPanel.setLayout(new GridBagLayout());

        GridBagConstraints gbc = new GridBagConstraints();
        gbc.gridwidth = 1;
        gbc.gridheight = 1;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.weightx = 1;
        gbc.insets = new Insets(3, 3, 3, 3);
        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.fill = GridBagConstraints.HORIZONTAL;

        JPanel paramPanel = new JPanel(new GridLayout(13, 2));
        paramPanel.setBorder(buildTitledBorder("Algorithm parameters"));

        paramPanel.add(createLabel("Iterations (1-10000)"));
        textIters = createTextField("10");
        paramPanel.add(textIters);

        paramPanel.add(createLabel("Gradient Magnitude scale >= 0"));
        textGMScale = createTextField("1.0");
        paramPanel.add(textGMScale);

        paramPanel.add(createLabel("Sigmoid Alpha"));
        textSigmoidAlpha = createTextField("-0.015");
        paramPanel.add(textSigmoidAlpha);

        paramPanel.add(createLabel("Sigmoid Beta"));
        textSigmoiBeta = createTextField("0.0125");
        paramPanel.add(textSigmoiBeta);

        paramPanel.add(createLabel("Sigmoid Min >= 0"));
        textSigmoidMin = createTextField("0");
        paramPanel.add(textSigmoidMin);

        paramPanel.add(createLabel("Sigmoid Max >= 0"));
        textSigmoidMax = createTextField("1.0");
        paramPanel.add(textSigmoidMax);

        paramPanel.add(createLabel("Max Coarse Iterations >= 1"));
        textCoarseMax = createTextField("1000");
        paramPanel.add(textCoarseMax);

        paramPanel.add(createLabel("Max distance >= 0"));
        textMaxDistance = createTextField("4.0");
        paramPanel.add(textMaxDistance);

        paramPanel.add(createLabel("Advection weight >= 0"));
        textAdvectionWeight = createTextField("0.0");
        paramPanel.add(textAdvectionWeight);

        paramPanel.add(createLabel("Propagation weight >= 0"));
        textPropagationWeight = createTextField("1.0");
        paramPanel.add(textPropagationWeight);

        paramPanel.add(createLabel("Curvature weight >= 0"));
        textCurvatureWeight = createTextField("0.05");
        paramPanel.add(textCurvatureWeight);

        paramPanel.add(createLabel("Laplacian weight >= 0"));
        textLaplacianWeight = createTextField("0.0");
        paramPanel.add(textLaplacianWeight);

        paramPanel.add(createLabel("Max Evolution >= 1"));
        textMaxEvolution = createTextField("50");
        paramPanel.add(textMaxEvolution);

        gbc.gridx = 0;
        gbc.gridy = 0;
        mainPanel.add(paramPanel, gbc);


        gbc.gridx = 0;
        gbc.gridy = 1;

        if ( image.getNDims() >= 3 )
        {
        	JPanel resPanel = new JPanel(new BorderLayout());
        	resPanel.setBorder(buildTitledBorder("Options"));

        	image25DCheckbox = new JCheckBox("Process each slice independently (2.5D)");
        	image25DCheckbox.setFont(serif12);
        	resPanel.add(image25DCheckbox, BorderLayout.SOUTH);
        	image25DCheckbox.setSelected(false);

        	if (image.getNDims() >= 3) { // if the source image is 3D then allow
        		image25DCheckbox.setEnabled(true);
        		image25DCheckbox.addItemListener(this);
        	} else {
        		image25DCheckbox.setEnabled(false);
        	}
        	mainPanel.add(resPanel, gbc);
        }
        
        JPanel buttonPanel = new JPanel();
        buildOKButton();
        buttonPanel.add(OKButton);
        buildCancelButton();
        buttonPanel.add(cancelButton);

        getContentPane().add(mainPanel, BorderLayout.CENTER);
        getContentPane().add(buttonPanel, BorderLayout.SOUTH);
        pack();
        setResizable(true);
        setVisible(true);

    }

	@Override
	public ActionMetadata getActionMetadata() {
		return new MipavActionMetadata() {
			public String getCategory() {
				return new String("Algorithms.Segmentation.ITK");
			}

			public String getDescription() {
				return new String("Computes levelset filters of an image.");
			}

			public String getDescriptionLong() {
				return new String("Computes levelset filters of an image.");
			}

			public String getShortLabel() {
				return new String("FastMarchingLevelSet");
			}

			public String getLabel() {
				return new String("FastMarching LevelSet");
			}

			public String getName() {
				return new String("FastMarching LevelSet");
			}
		};
	}

	@Override
	public ParameterTable createInputParameters() {
		final ParameterTable table = new ParameterTable();

		try {
			table.put(new ParameterExternalImage(AlgorithmParameters.getInputImageLabel(1)));
			table.put( new ParameterInt( "iterations" ) );
			table.put( new ParameterFloat( "gradientMagnitudeScale" ) );
			table.put( new ParameterFloat( "sigmoidAlpha" ) );
			table.put( new ParameterFloat( "sigmoidBeta" ) );
			table.put( new ParameterFloat( "sigmoidMin" ) );
			table.put( new ParameterFloat( "sigmoidMax" ) );
			table.put( new ParameterInt( "coarseIterationsMax" ) );
			table.put( new ParameterFloat( "maxDistance" ) );
			table.put( new ParameterFloat( "advectionWeight" ) );
			table.put( new ParameterFloat( "propagationWeight" ) );
			table.put( new ParameterFloat( "curvatureWeight" ) );
			table.put( new ParameterFloat( "laplacianWeight" ) );
			table.put( new ParameterInt( "evolutionIterationsMax" ) );
			table.put( new ParameterInt( "filterType" ) );
			table.put( new ParameterBoolean( "image25D" ) );
		} catch (final ParserException e) {
			// this shouldn't really happen since there isn't any real parsing going on...
			e.printStackTrace();
		}
		return table;
	}

	@Override
	public ParameterTable createOutputParameters() {
        final ParameterTable table = new ParameterTable();
        try {
        	table.put(new ParameterImage("resultImage"));
        } catch (final ParserException e) {
            // this shouldn't really happen since there isn't any real parsing going on...
            e.printStackTrace();
        }
        return table;
	}

	@Override
	public String getOutputImageName(String imageParamName) {
        Preferences.debug("Unrecognized output image parameter: " + imageParamName + "\n", Preferences.DEBUG_SCRIPTING);
		return null;
	}

	@Override
	public boolean isActionComplete() {
		return isComplete();
	}

	@Override
	protected void callAlgorithm() {
        if (image.getNDims() == 2) { // source image is 2D

            try {
            	
                // No need to make new image space because the user has choosen to replace the source image
                // Make the algorithm class
            	AlgorithmFastMarching fastMarchAlgo = new AlgorithmFastMarching(image, m_iFilterType, 
            			m_iIters, m_fGMScale, m_fSAlpha, m_fSBeta, m_fSMin, m_fSMax, m_iCoarseMax,
            			m_fMaxDistance, m_fAdvectionWeight,
            			m_fPropagationWeight, m_fCurvatureWeight, m_fLaplacianWeight, m_iEvolveMax, false);

                // This is very important. Adding this object as a listener allows the algorithm to
                // notify this object when it has completed of failed. See algorithm performed event.
                // This is made possible by implementing AlgorithmedPerformed interface
                fastMarchAlgo.addListener(this);
                
                createProgressBar(image.getImageName(), fastMarchAlgo);
                
                // Hide the dialog since the algorithm is about to run.
                setVisible(false);

                // Start the thread as a low priority because we wish to still have user interface.
                if (fastMarchAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                    MipavUtil.displayError("A thread is already running on this object");
                }
            } catch (OutOfMemoryError x) {
                MipavUtil.displayError("Dialog Diffusion: unable to allocate enough memory");

                return;
            }
        } else if (image.getNDims() == 3) {

            try {

                // Make algorithm
            	AlgorithmFastMarching fastMarchAlgo = new AlgorithmFastMarching(image, m_iFilterType, 
            			m_iIters, m_fGMScale, m_fSAlpha, m_fSBeta, m_fSMin, m_fSMax, m_iCoarseMax, 
            			m_fMaxDistance, m_fAdvectionWeight,
            			m_fPropagationWeight, m_fCurvatureWeight, m_fLaplacianWeight, m_iEvolveMax, m_bImage25D );

                // This is very important. Adding this object as a listener allows the algorithm to
                // notify this object when it has completed of failed. See algorithm performed event.
                // This is made possible by implementing AlgorithmedPerformed interface
            	fastMarchAlgo.addListener(this);
                createProgressBar(image.getImageName(), fastMarchAlgo);
                
                // Hide dialog
                setVisible(false);
                
                // Start the thread as a low priority because we wish to still have user interface work fast
                if (fastMarchAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                    MipavUtil.displayError("A thread is already running on this object");
                }
            } catch (OutOfMemoryError x) {
                MipavUtil.displayError("Dialog diffusion: unable to allocate enough memory");

                return;
            }
        }
		
	}

	@Override
	protected void setGUIFromParams() {
		image = scriptParameters.retrieveInputImage();
		parentFrame = image.getParentFrame();

		m_iIters = scriptParameters.getParams().getInt("iterations");
		m_fGMScale = scriptParameters.getParams().getFloat("gradientMagnitudeScale");
		m_fSAlpha = scriptParameters.getParams().getFloat("sigmoidAlpha");
		m_fSBeta = scriptParameters.getParams().getFloat("sigmoidBeta");
		m_fSMin = scriptParameters.getParams().getFloat("sigmoidMin");
		m_fSMax = scriptParameters.getParams().getFloat("sigmoidMax");
		m_iCoarseMax = scriptParameters.getParams().getInt("coarseIterationsMax");
		m_fMaxDistance = scriptParameters.getParams().getFloat("maxDistance");
		m_fAdvectionWeight = scriptParameters.getParams().getFloat("advectionWeight");
		m_fPropagationWeight = scriptParameters.getParams().getFloat("propagationWeight");
		m_fCurvatureWeight = scriptParameters.getParams().getFloat("curvatureWeight");
		m_fLaplacianWeight = scriptParameters.getParams().getFloat("laplacianWeight");
		m_iEvolveMax = scriptParameters.getParams().getInt("evolutionIterationsMax");
		m_iFilterType = scriptParameters.getParams().getInt("filterType");	
		m_bImage25D = scriptParameters.getParams().getBoolean("image25D");	
	}

	@Override
	protected void storeParamsFromGUI() throws ParserException {
		scriptParameters.storeInputImage(image);
		scriptParameters.getParams().put(ParameterFactory.newParameter("iterations", m_iIters ) );
		scriptParameters.getParams().put(ParameterFactory.newParameter("gradientMagnitudeScale", m_fGMScale) );
		scriptParameters.getParams().put(ParameterFactory.newParameter("sigmoidAlpha", m_fSAlpha) );
		scriptParameters.getParams().put(ParameterFactory.newParameter("sigmoidBeta", m_fSBeta) );
		scriptParameters.getParams().put(ParameterFactory.newParameter("sigmoidMin", m_fSMin) );
		scriptParameters.getParams().put(ParameterFactory.newParameter("sigmoidMax", m_fSMax) );
		scriptParameters.getParams().put(ParameterFactory.newParameter("coarseIterationsMax", m_iCoarseMax) );
		scriptParameters.getParams().put(ParameterFactory.newParameter("maxDistance", m_fMaxDistance) );
		scriptParameters.getParams().put(ParameterFactory.newParameter("advectionWeight", m_fAdvectionWeight) );
		scriptParameters.getParams().put(ParameterFactory.newParameter("propagationWeight", m_fPropagationWeight) );
		scriptParameters.getParams().put(ParameterFactory.newParameter("curvatureWeight", m_fCurvatureWeight) );
		scriptParameters.getParams().put(ParameterFactory.newParameter("laplacianWeight", m_fLaplacianWeight) );
		scriptParameters.getParams().put(ParameterFactory.newParameter("evolutionIterationsMax", m_iEvolveMax) );
		scriptParameters.getParams().put(ParameterFactory.newParameter("filterType", m_iFilterType) );	
		scriptParameters.getParams().put(ParameterFactory.newParameter("image25D", m_bImage25D) );	
	}

}
