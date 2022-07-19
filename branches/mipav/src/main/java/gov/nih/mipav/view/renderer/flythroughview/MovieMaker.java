package gov.nih.mipav.view.renderer.flythroughview;


import java.awt.*;

import java.io.*;

import javax.media.*;
import javax.media.control.*;
import javax.media.datasink.*;
import javax.media.format.*;
import javax.media.protocol.*;
import javax.media.util.*;


/**
 * This class creates AVIs (.avi) out of a list of JPEG files or out of an array of java.awt.Image objects.
 */
public class MovieMaker implements ControllerListener, DataSinkListener {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private boolean fileDone = false;

    /** DOCUMENT ME! */
    private boolean fileSuccess = true;

    /** Media locator for the output file. */
    private final MediaLocator outML;

    /** Video processor reference. */
    private final Processor processor;

    /** DOCUMENT ME! */
    private boolean stateTransitionOK = true;

    /** DOCUMENT ME! */
    private final Object waitFileSync = new Object();

    /** Synchonization control variables. */
    private final Object waitSync = new Object();

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Constructor.
     *
     * @param   width       frame width
     * @param   height      frame height
     * @param   frameRate   frame rate
     * @param   outputFile  AVI output file
     * @param   jpegFiles   jpeg images list.
     *
     * @throws  Exception  DOCUMENT ME!
     */
    public MovieMaker(int width, int height, int frameRate, File outputFile, File[] jpegFiles) throws Exception {
        this(outputFile, new ImageDataSource(width, height, frameRate, jpegFiles));
    }

    /**
     * Constructor.
     *
     * @param   width       frame width
     * @param   height      frame height
     * @param   frameRate   frame rate
     * @param   outputFile  AVI output file
     * @param   images      source images list.
     *
     * @throws  Exception  DOCUMENT ME!
     */
    public MovieMaker(int width, int height, int frameRate, File outputFile, Image[] images) throws Exception {
        this(outputFile, new ImageDataSource(width, height, frameRate, images));
    }

    /**
     * Constructor.
     *
     * @param   outputFile  AVI output file.
     * @param   ids         jpegs image data source.
     *
     * @throws  Exception  DOCUMENT ME!
     */
    private MovieMaker(File outputFile, ImageDataSource ids) throws Exception {
        this.processor = createProcessor(outputFile, ids);
        this.outML = new MediaLocator(outputFile.toURI().toURL());
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Controller Listener.
     *
     * @param  evt  controller event.
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
     * Method actually does the transcoding from JPeg images file to AVI.
     *
     * @throws  Exception  DOCUMENT ME!
     */
    public void makeMovie() throws Exception {

        // Query for the processor for supported formats.
        // Then set it on the processor.
        TrackControl[] tcs = processor.getTrackControls();
        Format[] f = tcs[0].getSupportedFormats();              

        if ((f == null) || (f.length == 0)) {
            throw new Exception("The mux does not support the input format: " + tcs[0].getFormat());
        }

        tcs[0].setFormat(f[0]);

        processor.realize();

        if (!waitForState(processor, Processor.Realized)) {
            throw new Exception("Failed to realize the processor.");
        }

        // Now, we'll need to create a DataSink.
        DataSink dsink = createDataSink(processor, outML);

        if (dsink == null) {
            throw new Exception("Failed to create a DataSink for the given output MediaLocator: " + outML);
        }

        dsink.addDataSinkListener(this);
        fileDone = false;

        // OK, we can now start the actual transcoding.
        processor.start();
        dsink.start();

        // Wait for EndOfStream event.
        waitForFileDone();

        // Cleanup.
        dsink.close();
        processor.removeControllerListener(this);

        // System.out.println( "...done processing." );
    }

    /**
     * Create the DataSink.
     *
     * @param   p      DOCUMENT ME!
     * @param   outML  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     *
     * @throws  IOException          DOCUMENT ME!
     * @throws  NoDataSinkException  DOCUMENT ME!
     */
    private static DataSink createDataSink(Processor p, MediaLocator outML) throws IOException, NoDataSinkException {
        DataSource ds = p.getDataOutput();

        if (ds == null) {
            throw new IOException("processor does not have an output DataSource");
        }

        // System.out.println( "- create DataSink for: " + outML );
        DataSink dsink = Manager.createDataSink(ds, outML);

        dsink.open();

        return dsink;
    }

    /**
     * Create video processor from the given source image files and output file.
     *
     * @param   outputFile  output file
     * @param   ids         ImageDataSource jpeg images file source.
     *
     * @throws  Exception                 DOCUMENT ME!
     * @throws  IOException               DOCUMENT ME!
     * @throws  IllegalArgumentException  DOCUMENT ME!
     *
     * @return  Processor the created vedio processor.
     */
    private Processor createProcessor(File outputFile, ImageDataSource ids) throws Exception {

        // System.out.println( "- create processor for the image datasource ..." );
        Processor p = Manager.createProcessor(ids);

        p.addControllerListener(this);

        // Put the Processor into configured state so we can set
        // some processing options on the processor.
        p.configure();

        if (!waitForState(p, p.Configured)) {
            throw new IOException("Failed to configure the processor.");
        }

        if (outputFile.getName().toLowerCase().endsWith(".mov")) {

            // Set the output content descriptor to QuickTime.
            p.setContentDescriptor(new ContentDescriptor(FileTypeDescriptor.QUICKTIME));
        } else if (outputFile.getName().toLowerCase().endsWith(".avi")) {

            // Set the output content descriptor to AVI (MSVIDEO).
            p.setContentDescriptor(new ContentDescriptor(FileTypeDescriptor.MSVIDEO));
        } else {
            throw new IllegalArgumentException("unsupported file extension: " + outputFile.getName());
        }

        return p;
    }

    /**
     * Block until file writing is done.
     *
     * @return  DOCUMENT ME!
     *
     * @throws  InterruptedException  DOCUMENT ME!
     */
    private boolean waitForFileDone() throws InterruptedException {

        synchronized (waitFileSync) {

            while (!fileDone) {
                waitFileSync.wait();
            }
        }

        return fileSuccess;
    }

    /**
     * Block until the processor has transitioned to the given state. Return false if the transition failed.
     *
     * @param   p      reference.
     * @param   state  processor transcoding state.
     *
     * @return  DOCUMENT ME!
     *
     * @throws  InterruptedException  DOCUMENT ME!
     */
    private boolean waitForState(Processor p, int state) throws InterruptedException {

        synchronized (waitSync) {

            while ((p.getState() < state) && stateTransitionOK) {
                waitSync.wait();
            }
        }

        return stateTransitionOK;
    }

    //~ Inner Classes --------------------------------------------------------------------------------------------------

    /**
     * The java.awt.Image-based source stream to go along with ImageDataSource. Not sure yet if this class works.
     */
    private static class AWTImageSourceStream implements PullBufferStream {

        /** DOCUMENT ME! */
        private boolean ended = false;

        /** DOCUMENT ME! */
        private final Image[] images;

        /** DOCUMENT ME! */
        private int nextImage = 0; // index of the next image to be read.

        /** Bug fix from Forums - next one line. */
        private long seqNo = 0;

        /** DOCUMENT ME! */
        private final VideoFormat videoFormat;

        /** DOCUMENT ME! */
        private final int width, height;

        /**
         * Creates a new AWTImageSourceStream object.
         *
         * @param  width      DOCUMENT ME!
         * @param  height     DOCUMENT ME!
         * @param  frameRate  DOCUMENT ME!
         * @param  images     DOCUMENT ME!
         */
        public AWTImageSourceStream(int width, int height, int frameRate, Image[] images) {
            this.width = width;
            this.height = height;
            this.images = images;

            // not sure if this is correct, especially the VideoFormat value
            this.videoFormat = new VideoFormat(VideoFormat.RGB, new Dimension(width, height), Format.NOT_SPECIFIED,
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
            return LENGTH_UNKNOWN;
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
         * Return the format of each video frame.
         *
         * @return  DOCUMENT ME!
         */
        public Format getFormat() {
            return videoFormat;
        }

        /**
         * This is called from the Processor to read a frame worth of video data.
         *
         * @param   buf  DOCUMENT ME!
         *
         * @throws  IOException       DOCUMENT ME!
         * @throws  RuntimeException  DOCUMENT ME!
         */
        public void read(final Buffer buf) throws IOException {

            try {

                // Check if we've finished all the frames.
                if (nextImage >= images.length) {

                    // We are done. Set EndOfMedia.
                    // System.out.println( "Done reading all images." );
                    buf.setEOM(true);
                    buf.setOffset(0);
                    buf.setLength(0);
                    ended = true;

                    return;
                }

                Image image = images[nextImage];

                nextImage++;

                // Open a random access file for the next image.
                // RandomAccessFile raFile = new RandomAccessFile(imageFile, "r");
                Buffer myBuffer = ImageToBuffer.createBuffer(image, videoFormat.getFrameRate());

                buf.copy(myBuffer);

                // Bug fix for AVI files from Forums ( next 3 lines).
                long time = (long) (seqNo * (1000 / videoFormat.getFrameRate()) * 1000000);

                buf.setTimeStamp(time);
                buf.setSequenceNumber(seqNo++);

                // buf.setOffset(0);
                // buf.setLength((int)raFile.length());
                // buf.setFormat(videoFormat);
                // buf.setFlags(buf.getFlags() | buf.FLAG_KEY_FRAME);

            } catch (Exception e) {

                // it's important to print the stack trace here because the
                // sun class that calls this method silently ignores
                // any Exceptions that get thrown
                e.printStackTrace();
                throw new RuntimeException(e);
            }
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

    // /////////////////////////////////////////////
    //
    // Inner classes.
    // /////////////////////////////////////////////


    /**
     * A DataSource to read from a list of JPEG image files or java.awt.Images, and turn that into a stream of JMF
     * buffers. The DataSource is not seekable or positionable.
     */
    private static class ImageDataSource extends PullBufferDataSource {

        /** DOCUMENT ME! */
        private final Time durTime;

        /** DOCUMENT ME! */
        private final PullBufferStream[] streams = new JpegSourceStream[1];

        /**
         * Constructor for creating movies out of jpegs.
         *
         * @param  width      DOCUMENT ME!
         * @param  height     DOCUMENT ME!
         * @param  frameRate  DOCUMENT ME!
         * @param  jpegFiles  DOCUMENT ME!
         */
        ImageDataSource(int width, int height, int frameRate, File[] jpegFiles) {
            streams[0] = new JpegSourceStream(width, height, frameRate, jpegFiles);
            this.durTime = new Time(jpegFiles.length / frameRate);
        }

        /**
         * Constructor for creating movies out of Images NOTE - this is all done IN MEMORY, so you'd better have enough.
         *
         * @param  width      DOCUMENT ME!
         * @param  height     DOCUMENT ME!
         * @param  frameRate  DOCUMENT ME!
         * @param  images     DOCUMENT ME!
         */
        ImageDataSource(int width, int height, int frameRate, Image[] images) {
            streams[0] = new AWTImageSourceStream(width, height, frameRate, images);
            this.durTime = new Time(images.length / frameRate);
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
         * DOCUMENT ME!
         *
         * @return  DOCUMENT ME!
         */
        public Time getDuration() {
            return durTime;
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
     * The jpeg-based source stream to go along with ImageDataSource.
     */
    private static class JpegSourceStream implements PullBufferStream {

        /** Bug fix from Forums - next one line. */
        long seqNo = 0;

        /** DOCUMENT ME! */
        private boolean ended = false;

        /** DOCUMENT ME! */
        private final File[] jpegFiles;

        /** DOCUMENT ME! */
        private int nextImage = 0; // index of the next image to be read.

        /** DOCUMENT ME! */
        private final VideoFormat videoFormat;

        /** DOCUMENT ME! */
        private final int width, height;

        /**
         * Creates a new JpegSourceStream object.
         *
         * @param  width      DOCUMENT ME!
         * @param  height     DOCUMENT ME!
         * @param  frameRate  DOCUMENT ME!
         * @param  jpegFiles  DOCUMENT ME!
         */
        public JpegSourceStream(int width, int height, int frameRate, File[] jpegFiles) {
            this.width = width;
            this.height = height;
            this.jpegFiles = jpegFiles;

            this.videoFormat = new VideoFormat(VideoFormat.JPEG, new Dimension(width, height), Format.NOT_SPECIFIED,
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
            return LENGTH_UNKNOWN;
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
            return videoFormat;
        }

        /**
         * This is called from the Processor to read a frame worth of video data.
         *
         * @param   buf  DOCUMENT ME!
         *
         * @throws  RuntimeException  DOCUMENT ME!
         */
        public void read(final Buffer buf) {

            try {

                // Check if we've finished all the frames.
                if (nextImage >= jpegFiles.length) {

                    // We are done. Set EndOfMedia.
                    // System.out.println( "Done reading all images." );
                    buf.setEOM(true);
                    buf.setOffset(0);
                    buf.setLength(0);
                    ended = true;

                    return;
                }

                File imageFile = jpegFiles[nextImage];

                nextImage++;

                // System.out.println( " - reading image file: " + imageFile );

                // Open a random access file for the next image.
                RandomAccessFile raFile = new RandomAccessFile(imageFile, "r");

                byte[] data = (byte[]) buf.getData();

                // Check to see the given buffer is big enough for the frame.
                if ((data == null) || (data.length < raFile.length())) {

                    // allocate larger buffer
                    data = new byte[(int) raFile.length()];
                    buf.setData(data);
                }

                // Read the entire JPEG image from the file.
                raFile.readFully(data, 0, (int) raFile.length());

                // System.out.println( " read " + raFile.length() + " bytes." );

                // Bug fix for AVI files from Forums ( next 3 lines).
                long time = (long) (seqNo * (1000 / videoFormat.getFrameRate()) * 1000000);

                buf.setTimeStamp(time);
                buf.setSequenceNumber(seqNo++);

                buf.setOffset(0);
                buf.setLength((int) raFile.length());
                buf.setFormat(videoFormat);
                buf.setFlags(buf.getFlags() | buf.FLAG_KEY_FRAME);

                // Close the random access file.
                raFile.close();
            } catch (Exception e) {

                // it's important to print the stack trace here because the
                // sun class that calls this method silently ignores
                // any IOExceptions that get thrown
                e.printStackTrace();
                throw new RuntimeException(e);
            }
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
