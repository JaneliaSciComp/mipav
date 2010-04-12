package gov.nih.mipav.view;

import java.awt.Color;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.event.ComponentEvent;
import java.awt.event.ComponentListener;
import java.util.EnumSet;
import java.util.Hashtable;

import javax.swing.BoundedRangeModel;
import javax.swing.JLabel;
import javax.swing.JSlider;

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

    public static final String TIME = "TIME";
    public static final String SLICE = "SLICE";
    public static final String CONTRAST = "CONTRAST";
    public static final String CUSTOM = "CUSTOM";
    
    public enum SliderType {
        
        TIME(ViewJSlider.TIME),
        SLICE(ViewJSlider.SLICE),
        CONTRAST(ViewJSlider.CONTRAST),
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
    private Dimension d;
    
    /**Holds the background BoundedRangeModel which describes the actual range of the data */
    private BoundedRangeModel brmBackground;
    
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
        
        for(int i=getMinimum()+intvlMajor; i<getMaximum(); i+=intvlMajor) {
            sliderLabels.put(i, createLabel(String.valueOf(i)));
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

    private void init(String typeStr) {
        type = SliderType.valueOf(typeStr);
        
        setDefaults();
    }
    
    public void setDefaults() {
        this.setMinorTickSpacing(type.getImpl().getMinorTickSpacing());
        this.setPaintTicks(type.getImpl().getPaintTicks());
        this.setPaintLabels(type.getImpl().getPaintLabels());
        this.setSnapToTicks(type.getImpl().getSnapToTicks());
        this.addComponentListener(new ViewJSliderResizeTool());
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
                if(!((JSlider)(e.getSource())).getSize().equals(d)) {
                    int value = getValue();
                    resizeSlider();
                    setValue(value);
                } 
            }
        }
    
        public void componentShown(ComponentEvent e) { }
    }
    
    private void resizeSlider() {
        double maxMinorTicks = 0.0;
        double maxMajorTicks = 0.0;
        if(getOrientation() == ViewJSlider.HORIZONTAL) {
            maxMinorTicks = getSize().getWidth()/6;
            maxMajorTicks = getSize().getWidth()/60;
        } else {
            maxMinorTicks = getSize().getHeight()/6;
            maxMajorTicks = getSize().getHeight()/60;
        }
        
        
        int intvlMinor = (int)Math.ceil((getMaximum()-getMinimum())/maxMinorTicks);
        int intvlMajor = (int)Math.ceil((getMaximum()-getMinimum())/maxMajorTicks);

        int i = 0, maxTrys = 20;
        while((intvlMajor % intvlMinor != 0 || (getMaximum()-getMinimum()+1)%intvlMajor != 0)
                && intvlMajor < getMaximum()-getMinimum() && i<maxTrys) {
            intvlMajor++;
            i++;
        }
        int oldIntvlMajor = getMajorTickSpacing();
        
        if(i<maxTrys) {
            if(intvlMajor < getMaximum() - getMinimum()) {
                setMajorTickSpacing(intvlMajor);
            }
            
            setMinorTickSpacing(intvlMinor);
            if(maxMajorTicks > 1) {
                setMajorTickSpacing(intvlMajor);
            }
            if(intvlMinor == 1) {
                setSnapToTicks(true);
            } else {
                setSnapToTicks(false);
            }
            
            if(intvlMajor != oldIntvlMajor) {
                setLabelTable(buildSliderLabels(intvlMajor));
            }
        }
    }
}
