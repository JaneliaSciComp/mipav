package gov.nih.mipav.plugins;

import gov.nih.mipav.model.structures.ModelImage;

/**
 * An interface for a plugin which reads/writes images.
 * @author mccreedy
 */
public interface PlugInFile extends PlugIn {
    /**
     * Read an image of the type handled by this plugin.
     * The plugin should get the path to the file from the user.
     */
    public void readImage();
    
    /**
     * Write an image out to the file type handled by this plugin.
     * The plugin should get the path to write to from the user.
     * @param image the image to write out
     */
    public void writeImage(ModelImage image);
    
    /**
     * Returns whether this plugin supports reading of images in its format.
     * @return whether this plugin supports reading of images
     */
    public boolean canReadImages();
    
    /**
     * Returns whether this plugin supports writing of images in its format.
     * @return whether this plugin supports writing of images
     */
    public boolean canWriteImages();
    
    /**
     * Checks whether a particular file extension is supported by this plugin.
     * @param ext the file extension to check
     * @return whether the extension is supported
     */
    public boolean isExtensionSupported(String ext);
}
