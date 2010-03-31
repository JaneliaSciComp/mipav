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

    public static final String TIME = "Time";
    public static final String SLICE = "Slice";
    public static final String CONTRAST = "Contrast";
    public static final String CUSTOM = "Custom";
    
    public enum SliderType {
        
        TIME(ViewJSlider.TIME),
        SLICE(ViewJSlider.SLICE),
        CONTRAST(ViewJSlider.CONTRAST);
        
        SliderType(String str) {
            if(str.equals(ViewJSlider.TIME)) {
                
            } else if(str.equals(ViewJSlider.SLICE)) {
                
            } else if(str.equals(ViewJSlider.CONTRAST)) {
                
            }
        }
        
    }

    /** Refers to the type of slider being used by MIPAV */
    private SliderType type;
    
    /** Holds the old dimension to minimize redrawing of slider table */
    private Dimension d;
    
    /**Holds the background BoundedRangeModel which describes the actual range of the data */
    private BoundedRangeModel brmBackground;
    
    /**
     * Creates a horizontal slider with the range and initial
     * value specified by type.
     * 
     * @param type See ViewJSlider.SliderTypes for possible pre-built types
     */
    public ViewJSlider(String type) {
        super();  
        init();
    }

    /**
     * Creates a horizontal slider with the range and initial
     * value specified by type, but the orientation is explicitly specified.
     * 
     * @param type See ViewJSlider.SliderTypes for possible pre-built types
     * @param orientation @see JSlider
     */
    public ViewJSlider(String type, int orientation) {
        super(orientation);
        init();
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
        init();
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
        init();
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
        init();
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
        init();
    }
    
    private void init() {
       
    }
    
    private void resizeSlider() {
        double maxTicks = getSize().getWidth()/6;
        
        int intvl = (int)Math.ceil(getMaximum()/maxTicks);
        setMinorTickSpacing(intvl);
        setSnapToTicks(false);
        setLabelTable(buildSliderLabels(getMinimum(), getMaximum()-1));
        setValue(1);
        setValue(0);
    }
    
    /**
     * Builds the slider labels for the slider.
     *
     * @param   min  Min value of slider
     * @param   max  Max value of slider.
     *
     * @return  Slider labels hash.
     */
    protected Hashtable buildSliderLabels(int min, int max) {
        Hashtable tImageSliderDictionary = new Hashtable();

        Font font12 = MipavUtil.font12;
        float rangeF = (max) / 4.0f;

        JLabel label1 = createLabel("0");
        tImageSliderDictionary.put(new Integer(0), label1);

        
        if ((max - min) > 3) {
            JLabel label2 = createLabel(Integer.toString(Math.round(rangeF * 2)-1));
            tImageSliderDictionary.put(max/2, label2);
        }

        JLabel label5 = createLabel(Integer.toString(max));
        tImageSliderDictionary.put(max, label5);

        return tImageSliderDictionary;
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
                    resizeSlider();
                    buildSliderLabels(0, getMaximum()-1);
                } 
            }
        }

        public void componentShown(ComponentEvent e) { }
    }
    
    /**
     * Helper method to create a label with the proper font and font color.
     *
     * @param   title  Text of the label.
     *
     * @return  New label.
     */
    private JLabel createLabel(String title) {
        JLabel label = new JLabel(title);
        label.setFont(MipavUtil.font12);
        label.setForeground(Color.black);

        return label;
    }
}
