package gov.nih.mipav.view.renderer.flythroughview;


import java.awt.*;

import java.io.*;

import java.util.*;

import javax.media.*;
import javax.media.control.*;
import javax.media.datasink.*;
import javax.media.format.*;
import javax.media.protocol.*;


/**
 * This program takes a list of JPEG image files and convert them into a QuickTime movie.
 */
public class JpegImagesToMovie implements ControllerListener, DataSinkListener {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    boolean fileDone = false;

    /** DOCUMENT ME! */
    boolean fileSuccess = true;

    /** DOCUMENT ME! */
    boolean stateTransitionOK = true;

    /** File write synchronization variables. */
    Object waitFileSync = new Object();

    /** DOCUMENT ME! */
    Object waitSync = new Object();

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Controller Listener.
     *
     * @param  evt  event.
     */
    public void controllerUpdate(ControllerEvent evt) {

        if ((evt instanceof ConfigureCompleteEvent) || (evt instanceof RealizeCompleteEvent) ||
                (evt instanceof PrefetchCompleteEvent)) {

            synchronized (waitSync) {
                stateTransitionOK = true;
                waitSync.notifyAll();
            }
        } else if (evt instanceof ResourceUnavailableEvent) {

            synchronized (waitSync) {
                stateTransitionOK = false;
                waitSync.notifyAll();
            }
        } else if (evt instanceof EndOfMediaEvent) {
            evt.getSourceController().stop();
            evt.getSourceController().close();
        }
    }

    /**
     * Event handler for the file writer.
     *
     * @param  evt  DOCUMENT ME!
     */
    public void dataSinkUpdate(DataSinkEvent evt) {

        if (evt instanceof EndOfStreamEvent) {

            synchronized (waitFileSync) {
                fileDone = true;
                waitFileSync.notifyAll();
            }
        } else if (evt instanceof DataSinkErrorEvent) {

            synchronized (waitFileSync) {
                fileDone = true;
                fileSuccess = false;
                waitFileSync.notifyAll();
            }
        }
    }

    /**
     * DOCUMENT ME!
     *
     * @param   width      DOCUMENT ME!
     * @param   height     DOCUMENT ME!
     * @param   frameRate  DOCUMENT ME!
     * @param   inFiles    DOCUMENT ME!
     * @param   outML      DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public boolean doIt(int width, int height, int frameRate, Vector inFiles, MediaLocator outML) {
        ImageDataSource ids = new ImageDataSource(width, height, frameRate, inFiles);

        Processor p;

        try {

            // System.err.println("- create processor for the image datasource ...");
            p = Manager.createProcessor(ids);
        } catch (Exception e) {
            System.err.println("Yikes!  Cannot create a processor from the data source.");

            return false;
        }

        p.addControllerListener(this);

        // Put the Processor into configured state so we can set
        // some processing options on the processor.
        p.configure();

        if (!waitForState(p, p.Configured)) {
            System.err.println("Failed to configure the processor.");

            return false;
        }

        // Set the output content descriptor to QuickTime.
        p.setContentDescriptor(new ContentDescriptor(FileTypeDescriptor.QUICKTIME));

        // Query for the processor for supported formats.
        // Then set it on the processor.
        TrackControl[] tcs = p.getTrackControls();
        Format[] f = tcs[0].getSupportedFormats();

        if ((f == null) || (f.length <= 0)) {
            System.err.println("The mux does not support the input format: " + tcs[0].getFormat());

            return false;
        }

        tcs[0].setFormat(f[0]);

        p.realize();

        if (!waitForState(p, p.Realized)) {
            System.err.println("Failed to realize the processor.");

            return false;
        }

        // Now, we'll need to create a DataSink.
        DataSink dsink;

        if ((dsink = createDataSink(p, outML)) == null) {
            System.err.println("Failed to create a DataSink for the given output MediaLocator: " + outML);

            return false;
        }

        dsink.addDataSinkListener(this);
        fileDone = false;

        // OK, we can now start the actual transcoding.
        try {
            p.start();
            dsink.start();
        } catch (IOException e) {
            System.err.println("IO error during processing");

            return false;
        }

        // Wait for EndOfStream event.
        waitForFileDone();

        // Cleanup.
        try {
            dsink.close();
        } catch (Exception e) { }

        p.removeControllerListener(this);

        return true;
    }

    /**
     * Create a media locator from the given string.
     *
     * @param   url  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    static MediaLocator createMediaLocator(String url) {

        MediaLocator ml;

        if ((url.indexOf(":") > 0) && ((ml = new MediaLocator(url)) != null)) {
            return ml;
        }

        if (url.startsWith(File.separator)) {

            if ((ml = new MediaLocator("file:" + url)) != null) {
                return ml;
            }
        } else {
            String file = "file:" + System.getProperty("user.dir") + File.separator + url;

            if ((ml = new MediaLocator(file)) != null) {
                return ml;
            }
        }

        return null;
    }

    /**
     * Create the data sink with the give processor and output media file location.
     *
     * @param   p      Processor reference.
     * @param   outML  MediaLocator reference.
     *
     * @return  DataSink the data sink object created
     */
    DataSink createDataSink(Processor p, MediaLocator outML) {

        DataSource ds;

        if ((ds = p.getDataOutput()) == null) {
            System.err.println("Something is really wrong: the processor does not have an output DataSource");

            return null;
        }

        DataSink dsink;

        try {
            dsink = Manager.createDataSink(ds, outML);
            dsink.open();
        } catch (Exception e) {
            System.err.println("Cannot create the DataSink: " + e);

            return null;
        }

        return dsink;
    }

    /**
     * Block until file writing is done.
     *
     * @return  DOCUMENT ME!
     */
    boolean waitForFileDone() {

        synchronized (waitFileSync) {

            try {

                while (!fileDone) {
                    waitFileSync.wait();
                }
            } catch (Exception e) { }
        }

        return fileSuccess;
    }

    /**
     * Block until the processor has transitioned to the given state. Return false if the transition failed.
     *
     * @param   p      DOCUMENT ME!
     * @param   state  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    boolean waitForState(Processor p, int state) {

        synchronized (waitSync) {

            try {

                while ((p.getState() < state) && stateTransitionOK) {
                    waitSync.wait();
                }
            } catch (Exception e) { }
        }

        return stateTransitionOK;
    }

    //~ Inner Classes --------------------------------------------------------------------------------------------------

    // /////////////////////////////////////////////
    //
    // Inner classes.
    // /////////////////////////////////////////////


    /**
     * A DataSource to read from a list of JPEG image files and turn that into a stream of JMF buffers. The DataSource
     * is not seekable or positionable.
     */
    class ImageDataSource extends PullBufferDataSource {

        /** DOCUMENT ME! */
        ImageSourceStream[] streams;

        /**
         * Creates a new ImageDataSource object.
         *
         * @param  width      DOCUMENT ME!
         * @param  height     DOCUMENT ME!
         * @param  frameRate  DOCUMENT ME!
         * @param  images     DOCUMENT ME!
         */
        ImageDataSource(int width, int height, int frameRate, Vector images) {
            streams = new ImageSourceStream[1];
            streams[0] = new ImageSourceStream(width, height, frameRate, images);
        }

        /**
         * DOCUMENT ME!
         */
        public void connect() { }

        /**
         * DOCUMENT ME!
         */
        public void disconnect() { }

        /**
         * Content type is of RAW since we are sending buffers of video frames without a container format.
         *
         * @return  DOCUMENT ME!
         */
        public String getContentType() {
            return ContentDescriptor.RAW;
        }

        /**
         * DOCUMENT ME!
         *
         * @param   type  DOCUMENT ME!
         *
         * @return  DOCUMENT ME!
         */
        public Object getControl(String type) {
            return null;
        }

        /**
         * DOCUMENT ME!
         *
         * @return  DOCUMENT ME!
         */
        public Object[] getControls() {
            return new Object[0];
        }

        /**
         * We could have derived the duration from the number of frames and frame rate. But for the purpose of this
         * program, it's not necessary.
         *
         * @return  DOCUMENT ME!
         */
        public Time getDuration() {
            return DURATION_UNKNOWN;
        }

        /**
         * DOCUMENT ME!
         *
         * @return  DOCUMENT ME!
         */
        public MediaLocator getLocator() {
            return null;
        }

        /**
         * Return the ImageSourceStreams.
         *
         * @return  DOCUMENT ME!
         */
        public PullBufferStream[] getStreams() {
            return streams;
        }

        /**
         * DOCUMENT ME!
         *
         * @param  source  DOCUMENT ME!
         */
        public void setLocator(MediaLocator source) { }

        /**
         * DOCUMENT ME!
         */
        public void start() { }

        /**
         * DOCUMENT ME!
         */
        public void stop() { }
    }


    /**
     * The source stream to go along with ImageDataSource.
     */
    class ImageSourceStream implements PullBufferStream {

        /** DOCUMENT ME! */
        boolean ended = false;

        /** DOCUMENT ME! */
        VideoFormat format;

        /** DOCUMENT ME! */
        Vector images;

        /** DOCUMENT ME! */
        int nextImage = 0; // index of the next image to be read.

        /** DOCUMENT ME! */
        int width, height;

        /**
         * Creates a new ImageSourceStream object.
         *
         * @param  width      DOCUMENT ME!
         * @param  height     DOCUMENT ME!
         * @param  frameRate  DOCUMENT ME!
         * @param  images     DOCUMENT ME!
         */
        public ImageSourceStream(int width, int height, int frameRate, Vector images) {
            this.width = width;
            this.height = height;
            this.images = images;

            format = new VideoFormat(VideoFormat.JPEG, new Dimension(width, height), Format.NOT_SPECIFIED,
                                     Format.byteArray, (float) frameRate);
        }

        /**
         * DOCUMENT ME!
         *
         * @return  DOCUMENT ME!
         */
        public boolean endOfStream() {
            return ended;
        }

        /**
         * DOCUMENT ME!
         *
         * @return  DOCUMENT ME!
         */
        public ContentDescriptor getContentDescriptor() {
            return new ContentDescriptor(ContentDescriptor.RAW);
        }

        /**
         * DOCUMENT ME!
         *
         * @return  DOCUMENT ME!
         */
        public long getContentLength() {
            return 0;
        }

        /**
         * DOCUMENT ME!
         *
         * @param   type  DOCUMENT ME!
         *
         * @return  DOCUMENT ME!
         */
        public Object getControl(String type) {
            return null;
        }

        /**
         * DOCUMENT ME!
         *
         * @return  DOCUMENT ME!
         */
        public Object[] getControls() {
            return new Object[0];
        }

        /**
         * Return the format of each video frame. That will be JPEG.
         *
         * @return  DOCUMENT ME!
         */
        public Format getFormat() {
            return format;
        }

        /**
         * This is called from the Processor to read a frame worth of video data.
         *
         * @param   buf  DOCUMENT ME!
         *
         * @throws  IOException  DOCUMENT ME!
         */
        public void read(Buffer buf) throws IOException {

            // Check if we've finished all the frames.
            if (nextImage >= images.size()) {

                // We are done.  Set EndOfMedia.
                // System.err.println("Done reading all images.");
                buf.setEOM(true);
                buf.setOffset(0);
                buf.setLength(0);
                ended = true;

                return;
            }

            String imageFile = (String) images.elementAt(nextImage);

            nextImage++;

            // Open a random access file for the next image.
            RandomAccessFile raFile;

            raFile = new RandomAccessFile(imageFile, "r");

            byte[] data = null;

            // Check the input buffer type & size.

            if (buf.getData() instanceof byte[]) {
                data = (byte[]) buf.getData();
            }

            // Check to see the given buffer is big enough for the frame.
            if ((data == null) || (data.length < raFile.length())) {
                data = new byte[(int) raFile.length()];
                buf.setData(data);
            }

            // Read the entire JPEG image from the file.
            raFile.readFully(data, 0, (int) raFile.length());

            // System.err.println("    read " + raFile.length() + " bytes.");

            buf.setOffset(0);
            buf.setLength((int) raFile.length());
            buf.setFormat(format);
            buf.setFlags(buf.getFlags() | buf.FLAG_KEY_FRAME);

            // Close the random access file.
            raFile.close();
        }

        /**
         * We should never need to block assuming data are read from files.
         *
         * @return  DOCUMENT ME!
         */
        public boolean willReadBlock() {
            return false;
        }
    }
}
