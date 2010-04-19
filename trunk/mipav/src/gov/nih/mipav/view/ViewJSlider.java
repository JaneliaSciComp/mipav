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
    
    /** Holds getMaximum()+getMinimum() throughout life of a ViewJSlider */
    private int maxPlusMin;
    
    /** Initialized in init(), holds the returned value of isPrime(maxPlusMin) */
    private boolean isTotalPrime;
    
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
        
        boundsChanged();
    }
    
    /**
     * When the JSliders extents have been changed in some way, this helper method
     * makes sure ViewJSlider variables are correctly set, it then calls resizeSlider()
     * to make sure the GUI is displaying properly.
     */
    private void boundsChanged() {
        maxPlusMin = getMaximum() + getMinimum();
        isTotalPrime = isPrime(maxPlusMin);
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
     * If it is a prime number, determine the maj tick spacing using the getBestTickSpacing method.
     * If there is room for all  minor ticks, then set minor tick spacing to 1.
     * If there is not room, first determine if the maj tick spacing is a prime number.
     * If it is a prime number, set the minor tick spacing to major tick spacing
     * If it is not a prime number, determine the minor tick spacing using the getBestTickSpacing method.
     * 
     * 
     * If it is not a prime number, set the maj tick spacing to 0
     * Determine the minor tick spacing using the getBestTickSpacing method
     */
    public void resizeSlider() {
    	double maxMinorTicks = 0.0;
        double maxMajorTicks = 0.0;
        int majTickSpacing,minTickSpacing;
        if(getOrientation() == ViewJSlider.HORIZONTAL) {
            maxMinorTicks = getSize().getWidth()/5;
            maxMajorTicks = getSize().getWidth()/50;
        } else {
            maxMinorTicks = getSize().getHeight()/5;
            maxMajorTicks = getSize().getHeight()/50;
        }
        
        if(maxPlusMin != getMaximum() + getMinimum()) {
            maxPlusMin = getMaximum() + getMinimum();
            isTotalPrime = isPrime(maxPlusMin);
        }

        if(maxPlusMin < maxMajorTicks) {
        	majTickSpacing = 1;
        	minTickSpacing = 1;
        	setMajorTickSpacing(majTickSpacing);
        	setMinorTickSpacing(minTickSpacing);
        	setLabelTable(buildSliderLabels(majTickSpacing));
        	setSnapToTicks(true);
        	return;
        }
        
        if(isTotalPrime) {
        	majTickSpacing = 0;
        	minTickSpacing = getBestTickSpacing(maxPlusMin + 1,maxMinorTicks);
			setMajorTickSpacing(majTickSpacing);
        	setMinorTickSpacing(minTickSpacing);
        	setLabelTable(buildSliderLabels(majTickSpacing));
        	if(minTickSpacing == 1) {
                setSnapToTicks(true);
            } else {
                setSnapToTicks(false);
            }
			return;
        	
        }else {
        	majTickSpacing = getBestTickSpacing(maxPlusMin, maxMajorTicks);
        	if(maxPlusMin < maxMinorTicks) {
        		minTickSpacing = 1;
        		setLabelTable(buildSliderLabels(majTickSpacing));
        		setMajorTickSpacing(majTickSpacing);
            	setMinorTickSpacing(minTickSpacing);
            	setSnapToTicks(true);
            	return;
        	}else {
        		if(isPrime(majTickSpacing)) {
        			minTickSpacing = majTickSpacing;
        			setMajorTickSpacing(majTickSpacing);
                	setMinorTickSpacing(minTickSpacing);
                	setLabelTable(buildSliderLabels(majTickSpacing));
                	if(minTickSpacing == 1) {
                        setSnapToTicks(true);
                    } else {
                        setSnapToTicks(false);
                    }
        			return;
        		}else {
        			minTickSpacing = getBestTickSpacing(maxPlusMin,maxMinorTicks);
        			if(majTickSpacing%minTickSpacing != 0) {
        				while(majTickSpacing%minTickSpacing != 0 && minTickSpacing <= majTickSpacing) {
        					minTickSpacing++;
        				}
        			}
        			setMajorTickSpacing(majTickSpacing);
                	setMinorTickSpacing(minTickSpacing);
                	setLabelTable(buildSliderLabels(majTickSpacing));
                	if(minTickSpacing == 1) {
                        setSnapToTicks(true);
                    } else {
                        setSnapToTicks(false);
                    }
        			return;
        		}
        	}
        }  
    }

    /**
     * @see javax.swing.JSlider#setExtent(int)
     */
    public void setExtent(int extent) {
        super.setExtent(extent);
        boundsChanged();
    }

    /**
    * @see javax.swing.JSlider#setMaximum(int)
    */
    public void setMaximum(int maximum) {
        super.setMaximum(maximum);
        boundsChanged();
    }

    /**
     * @see javax.swing.JSlider#setMinimum(int)
     */
    public void setMinimum(int minimum) {
        super.setMinimum(minimum);
        boundsChanged();
    }

    /**
     * @see javax.swing.JSlider#setModel(BoundedRangeModel)
     */
    public void setModel(BoundedRangeModel newModel) {
        super.setModel(newModel);
        boundsChanged();
    }


    /**
     * Gets the best tick spacing by finding the value that divides into the numb with the greatest numher that is 
     * still less than the maxNumber of ticks
     * @param num
     * @param maxNumTicks
     * @return Best tick spacing value
     */
    private int getBestTickSpacing(int num, double maxNumTicks) {
    	int bestTickSpacing = 1;
    
        for(int i=getMaximum();i>=1;i--) {
        	if(num%i==0) {
        		if(num/i < maxNumTicks) {
        			bestTickSpacing = i;
        		}
        	}
        }
    	return bestTickSpacing;
    	
    }
    
    

    
    /**
     * Helper function that determines if number is prime.
     * NEEDS TO NE MOVED TO MIPAVMATH AT SOME POINT
     * @param num Number to be determined if it is prime or not.
     * @return boolean telling whether it is prime or not.
     */
    private static boolean isPrime(int num) {
    	boolean isPrime = false;
    	int[] primes = {2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71,73,79,83,89,97,101,103,107,109,
    			113,127,131,137,139,149,151,157,163,167,173,179,181,191,193,197,199,211,223,227,229,233,239,
    			241,251,257,263,269,271,277,281,283,293,307,311,313,317,331,337,347,349,353,359,367,373,379,
    			383,389,397,401,409,419,421,431,433,439,443,449,457,461,463,467,479,487,491,499,503,509,521,523,
    			541,547,557,563,569,571,577,587,593,599,601,607,613,617,619,631,641,643,647,653,659,661,673,677,
    			683,691,701,709,719,727,733,739,743,751,757,761,769,773,787,797,809,811,821,823,827,829,839,
    			853,857,859,863,877,881,883,887,907,911,919,929,937,941,947,953,967,971,977,983,991,997};
    	for(int i=0;i<primes.length;i++) {
    		if(num == primes[i]) {
    			isPrime = true;
    			break;
    		}
    	}
    	return isPrime;
    }
}
