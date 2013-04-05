package gov.nih.mipav.view;



import java.awt.Color;
import java.awt.Dimension;
import java.awt.event.ComponentEvent;
import java.awt.event.ComponentListener;
import java.beans.PropertyChangeListener;
import java.util.Hashtable;

import javax.swing.BoundedRangeModel;
import javax.swing.JLabel;
import javax.swing.JSlider;
import javax.swing.SwingConstants;

/**
 * This class extends Java's JSlider to give basic solutions to common slider needs in 
 * image processing tasks.  The sliders contain methods for resizing and snapping that 
 * are not present in JSliders by default.  Another functional need common to MIPAV sliders
 * is the ability of the slider to reference a data range that is not displayed to the user.
 * For example, the MIPAV gui is always zero-based, but many images begin with slice 1.  This 
 * operation is represented as a background BoundedRangeModel that is not visible to the user
 * of a ViewJSlider.
 * 
 * @author senseneyj
 *
 */
public class ViewJSlider extends JSlider {

    @Override
	public int getValue() {
		// TODO Auto-generated method stub
		return super.getValue();
	}


	@Override
	public void setValue(int n) {
		// TODO Auto-generated method stub
		super.setValue(n);
	}

	public static final String TIME = "TIME";
    public static final String SLICE = "SLICE";
    public static final String CONTRAST = "CONTRAST";
    public static final String CUSTOM = "CUSTOM";
    public static final String OPACITY = "OPACITY";
    public static final String MAGNIFICATION = "MAGNIFICATION";
    
    public enum SliderType {
        
        TIME(ViewJSlider.TIME),
        SLICE(ViewJSlider.SLICE),
        CONTRAST(ViewJSlider.CONTRAST),
        OPACITY(ViewJSlider.OPACITY),
        MAGNIFICATION(ViewJSlider.MAGNIFICATION),
        CUSTOM(ViewJSlider.CUSTOM);    
        
        private ViewJSlider implement;
        private String str;
        
        SliderType(String str) {
            this.implement = new ViewJSlider();
            this.str = str;
            
            if(str.equals(ViewJSlider.TIME)) {
                implement.setOrientation(ViewJSlider.HORIZONTAL);
            } else if(str.equals(ViewJSlider.SLICE)) {
                implement.setOrientation(ViewJSlider.HORIZONTAL);
            } else if(str.equals(ViewJSlider.CONTRAST)) {
                implement.setOrientation(ViewJSlider.VERTICAL);
            } else if(str.equals(ViewJSlider.MAGNIFICATION)) {
                implement.setOrientation(ViewJSlider.HORIZONTAL);
            } else {
                implement.setOrientation(ViewJSlider.HORIZONTAL);
            }
            implement.setMinorTickSpacing(1);
            implement.setPaintTicks(true);
            implement.setPaintLabels(true);
            implement.setSnapToTicks(false);
        }
        
        protected ViewJSlider getImpl() {
            return implement;
        }
        
        public String toString() {
            return str;
        }
    }

    /** Refers to the type of slider being used by MIPAV */
    private SliderType type;
    
    /** Holds the old dimension to minimize redrawing of slider table */
    private Dimension dim;
    
    /**The closest number of pixels that major/minor ticks may be next to each other. Used to set GUI and create labels. */
    private double minAllowableMinorTickSpacing = 5;
    private double minAllowableMajorTickSpacing = 50;
    
    /** Default constructor **/
    private ViewJSlider() {}
    
    
    /**
     * Creates a horizontal slider with the range and initial
     * value specified by type.
     * 
     * @param type See ViewJSlider.SliderTypes for possible pre-built types
     */
    public ViewJSlider(String type) {
        super();  
        init(type);
    }

    /**
     * Creates a horizontal slider with the range and initial
     * value specified by type, but the orientation is explicitly specified.
     * 
     * @param type See ViewJSlider.SliderTypes for possible pre-built types
     * @param orientation @see JSlider
     */
    public ViewJSlider(String type, int maxBound) {
        super(0, maxBound);
        init(type);
    }

    /**
     * Creates a horizontal slider with the range and initial
     * value specified by type, but the range model is explicitly specified.
     * 
     * @param type See ViewJSlider.SliderTypes for possible pre-built types
     * @param brm @see JSlider
     */
    public ViewJSlider(String type, BoundedRangeModel brm) {
        super(brm);
        init(type);
    }

    /**
     * Creates a horizontal slider with the range and initial
     * value specified by type, but the min and max are explicitly to create
     * a Boundary Range model.
     * 
     * @param type See ViewJSlider.SliderTypes for possible pre-built types
     * @param min @see JSlider
     * @param max @see JSlider
     */
    public ViewJSlider(String type, int min, int max) {
        super(min, max);
        init(type);
    }

    /**
     * Creates a horizontal slider with the range and initial
     * value specified by type, but the min and max are explicitly to create
     * a Boundary Range model.  The initial value is also specified.
     * 
     * @param type See ViewJSlider.SliderTypes for possible pre-built types
     * @param min @see JSlider
     * @param max @see JSlider
     * @param value @see JSlider
     */
    public ViewJSlider(String type, int min, int max, int value) {
        super(min, max, value);
        init(type);
    }

    /**
     * Creates a horizontal slider with the range and initial
     * value specified by type, but the min and max are explicitly to create
     * a Boundary Range model.  The initial value and orientation
     * are also specified.
     * 
     * @param type See ViewJSlider.SliderTypes for possible pre-built types
     * @param orientation @see JSlider
     * @param min @see JSlider
     * @param max @see JSlider
     * @param value @see JSlider
     */
    public ViewJSlider(String type, int orientation, int min, int max, int value) {
        super(orientation, min, max, value);
        init(type);
    }
    
    /**
     * Builds the slider labels for the slider.
     *
     * @param   min  Min value of slider
     * @param   max  Max value of slider.
     *
     * @return  Slider labels hash.
     */
    protected Hashtable<Integer, JLabel> buildSliderLabels(int intvlMajor) {
        Hashtable<Integer, JLabel> sliderLabels = new Hashtable<Integer, JLabel>();
        sliderLabels.put(getMinimum(), createLabel(String.valueOf(getMinimum())));
        
        
        
        if(intvlMajor > 0) {
            for(int i=getMinimum()+intvlMajor; i<getMaximum(); i+=intvlMajor) {
                sliderLabels.put(i, createLabel(String.valueOf(i)));
            }   
            if(intvlMajor == getMaximum()) {
                int half = (int)Math.floor((getMaximum() + getMinimum())/2.0);
                sliderLabels.put(half, createLabel(String.valueOf(half)));
            }
        } else {
            int half = (int)Math.floor((getMaximum() + getMinimum())/2.0);
            sliderLabels.put(half, createLabel(String.valueOf(half)));
        }
            
        sliderLabels.put(getMaximum(), createLabel(String.valueOf(getMaximum())));
    
        return sliderLabels;
    }

    /**
     * Helper method to create a label with the proper font and font color.
     *
     * @param   title  Text of the label.
     *
     * @return  New label.
     */
    protected static JLabel createLabel(String title) {
        JLabel label = new JLabel(title);
        label.setFont(MipavUtil.font12);
        label.setForeground(Color.black);
    
        return label;
    }

    
    /**
     * Init method that creates slider
     * @param typeStr  The type of slider to construct
     */
    private void init(String typeStr) {
        type = SliderType.valueOf(typeStr);
        setDefaults();
        resizeSlider();
    }
    
    
    /**
     * Sets defaults
     */
    public void setDefaults() {
        this.setMinorTickSpacing(type.getImpl().getMinorTickSpacing());
        this.setPaintTicks(type.getImpl().getPaintTicks());
        this.setPaintLabels(type.getImpl().getPaintLabels());
        this.setSnapToTicks(type.getImpl().getSnapToTicks());
        this.setOrientation(type.getImpl().getOrientation());
        this.addComponentListener(new ViewJSliderResizeTool());
    }
    
    /**
     * Sets values for minimum allowable tick spacing depending on size of GUI.
     */
    private void buildMinimumTickSpacing() {
        int length = 0;
        if(getOrientation() == SwingConstants.VERTICAL) {
            length = getSize().height;
        } else {
            length = getSize().width;
        }
        
        if(length >= 150) {
            minAllowableMajorTickSpacing = 50.0;
            minAllowableMinorTickSpacing = 5.0;
        } else if(length >= 50) { //allow for small spacing for small GUIs
            minAllowableMajorTickSpacing = 25.0;
            minAllowableMinorTickSpacing = 3.0;
        } else { //don't allow for arbitrary small spacing in tiny GUIs
            minAllowableMajorTickSpacing = 50.0;
            minAllowableMinorTickSpacing = 5.0;
        }
    }

    /**
     * This class watches for GUI events that require slider marks to be reset.
     * 
     * @author senseneyj
     *
     */
    private class ViewJSliderResizeTool implements ComponentListener {
        
        public void componentHidden(ComponentEvent e) { }
        
        public void componentMoved(ComponentEvent e) { }
    
        public void componentResized(ComponentEvent e) {
            if(e.getSource() instanceof JSlider) {
                if(!((JSlider)(e.getSource())).getSize().equals(dim)) {
                    int value = getValue();
                    resizeSlider();
                    setValue(value);
                    dim = ((JSlider)(e.getSource())).getSize();
                } 
            }
        }
    
        public void componentShown(ComponentEvent e) { }
    }
    
    
    
    /**
     * Resize slider
     * 
     * The max number of major ticks and max number of minor ticks are determined using width/height of dialog.
     * The major tick spacing and minor tick spacing are determined as follows:
     * 
     * If the range of slices is less than max major ticks, then display ticks every 1 spacing
     * 
     * If it is greater than max major ticks, then determine if the range is a prime number
     * 
     * If it is not a prime number, determine the maj tick spacing using the getBestTickSpacing method.
     * If there is room for all  minor ticks, then set minor tick spacing to 1.
     * If there is not room, first determine if the maj tick spacing is a prime number.
     * If it is a prime number, set the minor tick spacing to major tick spacing
     * If it is not a prime number, determine the minor tick spacing using the getBestTickSpacing method.
     * 
     * 
     * If it is a prime number, set the maj tick spacing to 0
     * Determine the minor tick spacing using the getBestTickSpacing method
     */
    public void resizeSlider() {
        PropertyChangeListener[] tempListener = getPropertyChangeListeners();
        for(int i=0; i<tempListener.length; i++) {
            removePropertyChangeListener(tempListener[i]);
        }
        double maxMinorTicks = 0.0;
        double maxMajorTicks = 0.0;
        int majTickSpacing = 0,minTickSpacing = 0;
        buildMinimumTickSpacing();
        if(getOrientation() == ViewJSlider.HORIZONTAL) {
            maxMinorTicks = getSize().getWidth()/minAllowableMinorTickSpacing;
            maxMajorTicks = getSize().getWidth()/minAllowableMajorTickSpacing;
        } else {
            maxMinorTicks = getSize().getHeight()/minAllowableMinorTickSpacing;
            maxMajorTicks = getSize().getHeight()/minAllowableMajorTickSpacing;
        }
        int maxMinusMin = getMaximum() - getMinimum();
        boolean setMajor = false, setMinor = false;
        
        if(maxMinusMin <= maxMinorTicks) {
            minTickSpacing = 1;
            
            setMinor = true;
        }
        
        if(maxMinusMin <= maxMajorTicks) {
            majTickSpacing = 1;
            setMajor = true;
        }
        
        if(!setMajor) {
            majTickSpacing = getBestTickSpacing(maxMinusMin, maxMajorTicks);
            if(majTickSpacing == -1) {
                majTickSpacing = 0; //no valid majTickSpacing could be found, so label table will just create values at getMinimum(), half-way point, and getMaximum()
            } else {
                setMajor = true;
            }
        }
        
        if(setMajor) {
            minTickSpacing = getBestTickSpacing(majTickSpacing, majTickSpacing);
        } else {
            minTickSpacing = getBestTickSpacing(maxMinusMin, maxMinorTicks);
        }
        
        if(minTickSpacing == -1) {
            minTickSpacing = 0;
        }
        
        if(minTickSpacing == 1 || majTickSpacing == 1) {
            setSnapToTicks(true); //all image slices are represented by unique tick marks, so snap to them
        }

        if(minTickSpacing > 0) {
            setMinor = true;
        }
        
        if(setMajor) {
            setMajorTickSpacing(majTickSpacing);
        } else {
            setMajorTickSpacing(0); //major ticks will not be seen
        }
        
        if(setMinor) {
            setMinorTickSpacing(minTickSpacing);
        } else {
            setMinorTickSpacing(getMaximum() - getMinimum()); //minor ticks will only be seen at endpoints
        }
        setLabelTable(buildSliderLabels(majTickSpacing)); 

        for(int i=0; i<tempListener.length; i++) {
            addPropertyChangeListener(tempListener[i]);
        }
    }
    
    
    
    
    /**
     * Gets the best tick spacing by finding the value that divides into rangeNeeded with the greatest number that is 
     * still less than the maximum number of ticks that are allowed in the range
     * @param rangeNeeded the range covered by tick marks
     * @param maxNumTicks the number of tick marks that can exist in the range
     * @return Best tick spacing value
     */
    private int getBestTickSpacing(int rangeNeeded, double maxNumTicks) {
        int bestTickSpacing = 1;
        boolean converged = false;
    
        for(int i=rangeNeeded-1; i > 0; i--) {
            if(rangeNeeded%i==0 && rangeNeeded/i <= maxNumTicks) {
                    converged = true;
                    bestTickSpacing = i;
            }
        }
        if(converged) {
            return bestTickSpacing;
        } else {
            return -1;
        }
        
    }

    public double getMinAllowableMinorTickSpacing() {
        return minAllowableMinorTickSpacing;
    }

    public void setMinAllowableMinorTickSpacing(double minAllowableMinorTickSpacing) {
        this.minAllowableMinorTickSpacing = minAllowableMinorTickSpacing;
    }

    public double getMinAllowableMajorTickSpacing() {
        return minAllowableMajorTickSpacing;
    }

    public void setMinAllowableMajorTickSpacing(double minAllowableMajorTickSpacing) {
        this.minAllowableMajorTickSpacing = minAllowableMajorTickSpacing;
    }
    
    

    
    
}
