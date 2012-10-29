package gov.nih.mipav.view.renderer.WildMagic.Interface;


import gov.nih.mipav.view.CustomUIBuilder;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.ViewJColorChooser;
import gov.nih.mipav.view.ViewJComponentGraphAxes;
import gov.nih.mipav.view.ViewToolBarBuilder;
import gov.nih.mipav.view.ViewUserInterface;
import gov.nih.mipav.view.renderer.WildMagic.VolumeTriPlanarInterface;
import gov.nih.mipav.view.renderer.WildMagic.Render.VolumeImage;
import gov.nih.mipav.view.renderer.WildMagic.Render.MultiDimensionalTransfer.ClassificationWidget;
import gov.nih.mipav.view.renderer.WildMagic.Render.MultiDimensionalTransfer.VolumeImageMultiDimensionalTransfer;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.Frame;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.io.File;
import java.io.IOException;
import java.io.RandomAccessFile;
import java.util.Vector;

import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.ButtonGroup;
import javax.swing.JButton;
import javax.swing.JFileChooser;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JRadioButton;
import javax.swing.JScrollPane;
import javax.swing.JSlider;
import javax.swing.JToolBar;
import javax.swing.ScrollPaneConstants;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import javax.swing.filechooser.FileFilter;
import javax.swing.filechooser.FileNameExtensionFilter;

import WildMagic.LibFoundation.Mathematics.ColorRGBA;

import javax.media.opengl.awt.GLCanvas;
import com.jogamp.opengl.util.Animator;

/**
 * This panel contains the display panel for the 2D Histogram user-interface.
 *
 */
public class JPanelMultiDimensionalTransfer extends JInterfaceBase implements ChangeListener {


	/** Use serialVersionUID for interoperability. */
	private static final long serialVersionUID = 926266253314679850L;

	/** Color button for changing color. */
	protected JButton colorButton;
	/** Save the current multi-histogram widgets to file. */
	protected JButton saveButton;
	/** Read new multi-histogram widgets from file. */
	protected JButton loadButton;
	/** Alpha blend slider. */
	protected JSlider alphaSlider;
	/** Boundary emphasis slider slider. */
	private JSlider boundaryEmphasisSlider;
	/** Button group for the widget type: */
	private ButtonGroup m_kGroup = new ButtonGroup();

	/** The scroll pane holding the panel content. Useful when the screen is small. */
	private JScrollPane scroller;

	/** Scroll panel that holding the all the control components. */
	private JPanel scrollPanel;

	/** Displays the 2D Histogram and widgets: */
	private VolumeImageMultiDimensionalTransfer m_kMultiHistogram;
	/** Panel containing the 2D Histogram display canvas: */
	private JPanel histogramPanel;
	/** Graph axes helper classes for displaying the axes of the 2D Histogram:  */
	private ViewJComponentGraphAxes imageAxis;
	private ViewJComponentGraphAxes gmAxis;
	private JPanel helpPanel = new JPanel(new GridBagLayout());

	private final static int Circle = 0;
	private final static int Square = 1;
	private final static int Triangle = 2;


	/**
	 * Creates new dialog for turning bounding box frame on and off.
	 * @param  parent  parent frame.
	 */
	public JPanelMultiDimensionalTransfer( GLCanvas canvas, VolumeTriPlanarInterface parent,
			Animator kAnimator, VolumeImage kVolumeImage) {
		m_kVolumeViewer = parent;
		m_kMultiHistogram = new VolumeImageMultiDimensionalTransfer( canvas, parent, kVolumeImage);
		m_kMultiHistogram.SetAnimator(kAnimator);
		m_kMultiHistogram.SetInterface(this);

		init(!kVolumeImage.GetImage().isColorImage());
	}

	/* (non-Javadoc)
	 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
	 */
	public void actionPerformed(ActionEvent event) {
		Object source = event.getSource();
		if ( source == colorButton )
		{
			colorChooser = new ViewJColorChooser(new Frame(), "Pick surface color", new OkColorListener(colorButton),
					new CancelListener());
		}
		else if ( source == saveButton )
		{
			String fileName = getMultiHistogramFile(true);
			if ( fileName != null )
			{
				saveMultiHistograms(fileName);
			}
		}
		else if ( source == loadButton )
		{
			String fileName = getMultiHistogramFile(false);
			if ( fileName != null )
			{
				loadMultiHistograms(fileName);
			}
		}
		else if ( event.getActionCommand().equals("CircleWidget") )
		{
			m_kMultiHistogram.setWidget( "Circle" );
			updateHelp(Circle);
		}
		else if ( event.getActionCommand().equals("SquareWidget") )
		{
			m_kMultiHistogram.setWidget( "Square" );
			updateHelp(Square);
		}
		else if ( event.getActionCommand().equals("TriWidget") )
		{
			m_kMultiHistogram.setWidget( "Triangle" );
			updateHelp(Triangle);
		}
		else
		{
			m_kMultiHistogram.update( event.getActionCommand() );
		}
	}

	/**
	 * Dispose memory.
	 */
	public void disposeLocal()
	{
		colorButton = null;
		colorChooser = null;
		boundaryEmphasisSlider = null;
		m_kGroup = null;
		scroller = null;
		scrollPanel = null;
		if ( m_kMultiHistogram != null )
		{
			m_kMultiHistogram.dispose();
			m_kMultiHistogram = null;
		}
		super.dispose();
	}

	/**
	 * Sets the minimum and maximum values of the ModelImage and the Gradient Magnitude image
	 * for displaying the graph axes of the 2D Histogram. 
	 * @param imageMin
	 * @param imageMax
	 * @param gmMin
	 * @param gmMax
	 */
	public void setMinMax( float imageMin, float imageMax, float gmMin, float gmMax )
	{
		imageAxis.setMinMax( imageMin, imageMax );
		gmAxis.setMinMax( gmMin, gmMax );
	}


	/**
	 * Access to the 2D Histogram display class so it can be updated from outside this class.
	 * @return
	 */
	public VolumeImageMultiDimensionalTransfer getHistogram()
	{
		return m_kMultiHistogram;
	}

	/**
	 * Resizing the control panel with ViewJFrameVolumeView's frame width and height.
	 *
	 * @param  panelWidth   int width
	 * @param  frameHeight  int height
	 */
	public void resizePanel(int panelWidth, int frameHeight) {
		if ( m_kMultiHistogram != null )
		{
			int iWidth = Math.max( panelWidth, m_kMultiHistogram.GetWidth() );
			int iHeight = Math.max( frameHeight - 40, m_kMultiHistogram.GetHeight() );
			scroller.setPreferredSize(new Dimension(iWidth, iHeight));
			scroller.setSize(new Dimension(iWidth, iHeight));
			scroller.revalidate();
			histogramPanel.setSize(new Dimension(iWidth, histogramPanel.getHeight()));
		}
	}

	/* (non-Javadoc)
	 * @see gov.nih.mipav.view.renderer.WildMagic.Interface.JInterfaceBase#setButtonColor(javax.swing.JButton, java.awt.Color)
	 */
	public void setButtonColor(JButton _button, Color _color)
	{
		super.setButtonColor( _button, _color );
		float fAlpha = alphaSlider.getValue()/100.0f;
		m_kMultiHistogram.setColor( new ColorRGBA( _color.getRed()/255.0f, _color.getGreen()/255.0f, _color.getBlue()/255.0f, fAlpha ) );
	}

	/* (non-Javadoc)
	 * @see javax.swing.event.ChangeListener#stateChanged(javax.swing.event.ChangeEvent)
	 */
	public void stateChanged(ChangeEvent e) {
		Object source = e.getSource();

		if (source == alphaSlider) {
			float fAlpha = alphaSlider.getValue()/100.0f;
			m_kMultiHistogram.setAlpha( fAlpha );
		}


		if (source == boundaryEmphasisSlider) {
			float fAlpha = boundaryEmphasisSlider.getValue()/100.0f;
			m_kMultiHistogram.setBoundary( fAlpha );
		}
	}

	/**
	 * Calls update display on the 2D Histogram class.
	 */
	public void update()
	{
		m_kMultiHistogram.display();
	}

	/* (non-Javadoc)
	 * @see gov.nih.mipav.view.renderer.WildMagic.Interface.JInterfaceBase#updateColorButton(float[], float)
	 */
	public void updateColorButton( float[] afColor, float fColor )
	{
		colorButton.setBackground( new Color( afColor[0], afColor[1], afColor[2]) );
		alphaSlider.setValue( (int)(afColor[3] * 100) );
		boundaryEmphasisSlider.setValue( (int)(fColor * 100) );
	}


	public synchronized VolumeImageMultiDimensionalTransfer getM_kMultiHistogram() {
		return m_kMultiHistogram;
	}

	/**
	 * Initializes GUI components.
	 */
	private void init( boolean useBoundaryEmphasis ) {
		GridBagConstraints kGBC = new GridBagConstraints();
		GridBagLayout kGrid = new GridBagLayout();
		kGBC.gridx = 0;
		kGBC.gridy = 0;
		kGBC.insets = new Insets(5,5,5,5);
		kGBC.anchor = GridBagConstraints.WEST;
		JPanel buttonPanel = new JPanel( kGrid );
		buttonPanel.setBorder(buildTitledBorder("Options"));
		buttonPanel.add( new JLabel( "Select Widget Type: "), kGBC );
		kGBC.gridx++;
		JRadioButton kCircle = new JRadioButton( "Circle", true );
		kCircle.addActionListener(this);
		kCircle.setActionCommand("CircleWidget");
		buttonPanel.add( kCircle, kGBC);
		kGBC.gridx++;
		JRadioButton kSquare = new JRadioButton( "Square", true );
		kSquare.addActionListener(this);
		kSquare.setActionCommand("SquareWidget");
		buttonPanel.add( kSquare, kGBC );
		kGBC.gridx++;
		JRadioButton kTriangle = new JRadioButton( "Triangle", true );
		kTriangle.addActionListener(this);
		kTriangle.setActionCommand("TriWidget");
		buttonPanel.add( kTriangle, kGBC);
		m_kGroup.add(kCircle);
		m_kGroup.add(kSquare);
		m_kGroup.add(kTriangle);
		kSquare.setSelected(true);

		kGBC.gridx = 0;
		kGBC.gridy++;
		alphaSlider = new JSlider();
		alphaSlider.addChangeListener(this);
		buttonPanel.add( new JLabel( "Histogram opacity: "), kGBC );
		kGBC.gridx++;
		kGBC.gridwidth = 2;
		buttonPanel.add( alphaSlider, kGBC );
		kGBC.insets = new Insets(5,5,10,5);
		kGBC.gridwidth = 1;
		kGBC.gridx = 0;
		kGBC.gridy++;
		boundaryEmphasisSlider = new JSlider(0, 100, 0);
		boundaryEmphasisSlider.addChangeListener(this);
		boundaryEmphasisSlider.setEnabled(useBoundaryEmphasis);
		buttonPanel.add( new JLabel( "Boundary Emphasis Slider: "), kGBC );
		kGBC.gridx++;
		kGBC.gridwidth = 2;
		buttonPanel.add( boundaryEmphasisSlider, kGBC );

		histogramPanel = new JPanel(new BorderLayout());
		histogramPanel.setBorder(buildTitledBorder("2D Histogram Visualization Tool"));
		JPanel canvasPanel = new JPanel(new BorderLayout());
		canvasPanel.add(m_kMultiHistogram.GetCanvas(), BorderLayout.CENTER);
		canvasPanel.setPreferredSize(new Dimension(256, 256));
		canvasPanel.setBackground(Color.white);
		histogramPanel.add( canvasPanel, BorderLayout.CENTER );
		imageAxis = new ViewJComponentGraphAxes( ViewJComponentGraphAxes.X_AXIS, ViewJComponentGraphAxes.TOP, 
				256 + 160, 50, "Image Intensities", 80 );
		histogramPanel.add( imageAxis, BorderLayout.SOUTH );
		gmAxis = new ViewJComponentGraphAxes( ViewJComponentGraphAxes.Y_AXIS,  ViewJComponentGraphAxes.LEFT, 
				80, 256, "Gradient Magnitude", 0 );
		histogramPanel.add( gmAxis, BorderLayout.EAST );
		histogramPanel.add( new ViewJComponentGraphAxes( ViewJComponentGraphAxes.Y_AXIS, ViewJComponentGraphAxes.RIGHT, 
				80, 256, null, 0 ), BorderLayout.WEST);

		// Scroll panel that hold the control panel layout in order to use JScrollPane
		scrollPanel = new JPanel(new BorderLayout());

		scroller = new JScrollPane(scrollPanel, ScrollPaneConstants.VERTICAL_SCROLLBAR_AS_NEEDED,
				ScrollPaneConstants.HORIZONTAL_SCROLLBAR_AS_NEEDED);
		mainPanel = new JPanel(new BorderLayout());

		helpPanel.setBorder(buildTitledBorder("Help"));
		GridBagConstraints gbc = new GridBagConstraints();
		gbc.gridx = 0;
		gbc.gridy = 0;
		gbc.insets = new Insets(5,5,5,5);
		gbc.anchor = GridBagConstraints.WEST;
		helpPanel.add( new JLabel( "- To insert a new widget, right-mouse click in the 2D Histogram "), gbc );
		gbc.gridy++;
		helpPanel.add( new JLabel( "- To move the square, drag the interior of the widget "), gbc );
		gbc.gridy++;
		helpPanel.add( new JLabel( "- To resize the square, drag the blue control points "), gbc );
		gbc.gridy++;
		helpPanel.add( new JLabel( "- To control intensity distribution, drag the green control point"), gbc );
		gbc.gridy++;
		helpPanel.add( new JLabel( "- To delete a widget, select it and then press the delete key"), gbc );
		gbc.gridy++;
		helpPanel.add( new JLabel( ""), gbc );
		gbc.gridy++;
		


		ViewToolBarBuilder toolBarObj = new ViewToolBarBuilder(this);

		JToolBar lutToolBar = toolBarObj.buildLUTToolBarTop();
		colorButton = toolBarObj.buildButton( "", "Change histogram color", CustomUIBuilder.PARAM_PAINT_COLOR.getIconBase() );
		saveButton = toolBarObj.buildButton( "save", "Save histograms", "save" );
		loadButton = toolBarObj.buildButton( "load", "Change histogram color", "open" );
		lutToolBar.add( colorButton );
		lutToolBar.add( saveButton );
		lutToolBar.add( loadButton );

		Box contentBox = new Box(BoxLayout.Y_AXIS);
		contentBox.setBorder(BorderFactory.createEmptyBorder(5, 5, 5, 5));
		contentBox.add(lutToolBar);
		contentBox.add(histogramPanel);
		contentBox.add(buttonPanel);
		contentBox.add(helpPanel);

		scrollPanel.add(contentBox, BorderLayout.NORTH);


		mainPanel.add(scroller, BorderLayout.NORTH);
	}
	private void updateHelp( int iType )
	{
		if ( iType == Circle )
		{
			Component[] components = helpPanel.getComponents();
			if ( components.length > 5 )
			{
				((JLabel)helpPanel.getComponent(0)).setText( "- To insert a new widget, right-mouse click in the 2D Histogram ");
				((JLabel)helpPanel.getComponent(1)).setText( "- To move the circle, drag the interior of the widget ");
				((JLabel)helpPanel.getComponent(2)).setText( "- To resize the circle, drag the blue control point ");
				((JLabel)helpPanel.getComponent(3)).setText( "- To control intensity distribution, drag the green control point");
				((JLabel)helpPanel.getComponent(4)).setText( "- To delete a widget, select it and then press the delete key");
				((JLabel)helpPanel.getComponent(5)).setText( "");
			}
		}
		else if ( iType == Square )
		{
			Component[] components = helpPanel.getComponents();
			if ( components.length > 5 )
			{
				((JLabel)helpPanel.getComponent(0)).setText( "- To insert a new widget, right-mouse click in the 2D Histogram ");
				((JLabel)helpPanel.getComponent(1)).setText( "- To move the square, drag the interior of the widget ");
				((JLabel)helpPanel.getComponent(2)).setText( "- To resize the square, drag the blue control points ");
				((JLabel)helpPanel.getComponent(3)).setText( "- To control intensity distribution, drag the green control point");
				((JLabel)helpPanel.getComponent(4)).setText( "- To delete a widget, select it and then press the delete key");
				((JLabel)helpPanel.getComponent(5)).setText( "");
			}
		}
		else
		{
			Component[] components = helpPanel.getComponents();
			if ( components.length > 5 )
			{
				((JLabel)helpPanel.getComponent(0)).setText( "- To insert a new widget, right-mouse click in the 2D Histogram ");
				((JLabel)helpPanel.getComponent(1)).setText( "- To move the triangle, drag the bottom blue control point ");
				((JLabel)helpPanel.getComponent(2)).setText( "- To resize the triangle, drag the top blue control point ");
				((JLabel)helpPanel.getComponent(3)).setText( "- To shear the triangle, drag inside the triangle ");
				((JLabel)helpPanel.getComponent(4)).setText( "- To control intensity distribution, drag the green control point");
				((JLabel)helpPanel.getComponent(5)).setText( "- To delete a widget, select it and then press the delete key");
			}
		}
	}

    private String getMultiHistogramFile(boolean bSave) {
        final JFileChooser chooser = new JFileChooser();
        chooser.setMultiSelectionEnabled(false);

        // TODO: Use FileNameExtensionFilter introduced in 1.6
        FileNameExtensionFilter kFileExtFilter = new FileNameExtensionFilter( "multi-histogram", "mh" );
        chooser.addChoosableFileFilter(kFileExtFilter);

        final FileFilter kFileFilter = new FileFilter() {
            public boolean accept(File f) {
                if (f.getName().toLowerCase().endsWith(".mh")) {
                    return true;
                }
				return false;
            }

            public String getDescription() {
                return "multi-histogram";
            }
        };
        chooser.addChoosableFileFilter(kFileFilter);

        if (ViewUserInterface.getReference().getDefaultDirectory() != null) {
            chooser.setCurrentDirectory(new File(ViewUserInterface.getReference().getDefaultDirectory()));
        } else {
            chooser.setCurrentDirectory(new File(System.getProperties().getProperty("user.dir")));
        }

        if (bSave && JFileChooser.APPROVE_OPTION != chooser.showSaveDialog(null)) {
            return null;
        }
        else if (!bSave && JFileChooser.APPROVE_OPTION != chooser.showOpenDialog(null)) {
            return null;
        }
        String kFile = chooser.getSelectedFile().getName();
        final String kDir = String.valueOf(chooser.getCurrentDirectory()) + File.separatorChar;
        chooser.setVisible(false);
        if ( !kFile.endsWith(".mh")) {
            kFile = kFile.concat(".mh");
        }
        return new String(kDir + kFile);
    }
    
    private void loadMultiHistograms(String fileName)
    {
    	m_kMultiHistogram.load(fileName);    	
    }
    
    private void saveMultiHistograms(String fileName)
    {
    	m_kMultiHistogram.save(fileName);
    }
}
