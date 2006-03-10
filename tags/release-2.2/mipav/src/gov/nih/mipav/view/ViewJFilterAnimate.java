package gov.nih.mipav.view;

import java.awt.image.*;

public class ViewJFilterAnimate extends RGBImageFilter {
    private int brightness;
    private float contrast;
    
    public ViewJFilterAnimate(int brightness, float contrast) {
        if (brightness < -255 || brightness > 255) {
            throw new IllegalArgumentException("Bad brightness argument");
        }
        else if (contrast < 0.0f || contrast > 255.0f) {
            throw new IllegalArgumentException("Bad contrast argument");
        }
        this.brightness = brightness;
        this.contrast = contrast;
        canFilterIndexColorModel = true;
    }
    
    public int brightness() {
        return brightness;
    }
    
    public void brightness(int brightness) {
        this.brightness = brightness;
    }
    
    public float contrast() {
        return contrast;
    }
    
    public void contrast(float contrast) {
        this.contrast = contrast;
    }
    
    public int filterRGB(int x, int y, int rgb) {
        DirectColorModel cm = (DirectColorModel)ColorModel.getRGBdefault();
        
        int alpha = cm.getAlpha(rgb);
        int red = cm.getRed(rgb);
        int green = cm.getGreen(rgb);
        int blue = cm.getBlue(rgb);
        
        red = Math.max(0,Math.min((int)(brightness + red*contrast),255));
        green = Math.max(0,Math.min((int)(brightness + green*contrast),255));
        blue = Math.max(0,Math.min((int)(brightness + blue*contrast),255));
        
        alpha = alpha << 24;
        red = red << 16;
        green = green << 8;
        
        return alpha | red | green | blue;
    }
}
    
    
        