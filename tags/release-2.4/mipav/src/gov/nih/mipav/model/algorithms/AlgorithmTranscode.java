package gov.nih.mipav.model.algorithms;


import java.io.*;
import java.awt.*;
import java.net.*;
import javax.media.*;
import javax.media.control.TrackControl;
import javax.media.control.QualityControl;
import javax.media.Format;
import javax.media.format.*;
import javax.media.datasink.*;
import javax.media.protocol.*;
import javax.media.protocol.DataSource;

import gov.nih.mipav.view.ViewJProgressBar;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.Preferences;


/**
 * <p>Title: </p>
 * <p>Description: </p>
 * @author not attributable
 * @version 1.0
 */
public class AlgorithmTranscode extends AlgorithmBase
    implements ControllerListener, DataSinkListener {

    public static final int TRANSCODE_RGB = 0;
    public static final int TRANSCODE_8_BIT_RLE = 1;
    public static final int TRANSCODE_QT = 2;
    public static final int TRANSCODE_MJPG = 3;
    public static final int TRANSCODE_MP42 = 4;
    public static final int TRANSCODE_MPG4 = 5;
    public static final int TRANSCODE_DIVX = 6;
    public static final int TRANSCODE_IV32 = 7;
    public static final int TRANSCODE_IV41 = 8;
    public static final int TRANSCODE_IV50 = 9;
    public static final int TRANSCODE_DX50 = 10;
    public static final int TRANSCODE_CVID = 11;

    private Object waitSync = new Object();
    private boolean stateTransitionOK = true;
    private boolean fileDone = false;

    private Object waitFileSync = new Object();
    private boolean fileSuccess = true;
    private Processor p = null;

    private String outputName;
    private URL inputURL;
    private int compressionType;

    private float quality = 0.8f;

    public boolean updateValImmed = true;

    public AlgorithmTranscode( URL inputURL, String outName,
            int compressionType ) {
        this.compressionType = compressionType;
        this.inputURL = inputURL;

        if ( compressionType == TRANSCODE_QT ) {
            if ( outName.endsWith( ".avi" ) || outName.endsWith( ".AVI" ) ) {
                outName = outName.substring( 0, outName.length() - 3 ) + "mov";
            } else if ( !outName.endsWith( ".mov" ) && !outName.endsWith( ".MOV" ) ) {
                outName += ".mov";
            }
        }
        this.outputName = outName;

    }

    public void setQuality( float quality ) {
        // System.err.println("Setting quality to: " + quality);
        this.quality = quality;
    }

    public void runAlgorithm() {

        File outputFile = null;
        MediaLocator iml = null, oml = null;

        iml = new MediaLocator( inputURL );
        outputFile = new File( outputName );
        // System.err.println("output file after creation: " + outputFile.getPath());
        try {
            oml = new MediaLocator( outputFile.toURI().toURL() );
        } catch ( MalformedURLException ex ) {//
        }

        try {
            p = Manager.createProcessor( iml );
        } catch ( Exception e ) {
            MipavUtil.displayError( "Yikes!  Cannot create a processor from the given url: " + e );
            return;
        }

        p.addControllerListener( this );

        // Put the Processor into configured state.
        p.configure();
        if ( !waitForState( p, p.Configured ) ) {
            MipavUtil.displayError( "Failed to configure the processor." );
            return;
        }

        String ext = "avi";

        if ( compressionType == TRANSCODE_QT ) {
            ext = "mov";
        }
        String tp = com.sun.media.MimeManager.getMimeType( ext );

        p.setContentDescriptor( new FileTypeDescriptor( ContentDescriptor.mimeTypeToPackageName( tp ) ) );

        TrackControl[] tcs = p.getTrackControls();
        String encodeStr = "Transcoding to ";

        /**
         * The length of the TrackControl tcs tells us if there is an audio track
         * (always comes first)... so if there is an audio track, we want to skip
         * it and go directly to the index of the video
         */
        int index = 0;

        if ( tcs.length > 1 ) {
            String trackName = null;

            for ( int i = 0; i < tcs.length; i++ ) {
                trackName = tcs[i].getFormat().toString();
                if ( trackName.startsWith( "LIN" ) || trackName.startsWith( "ULAW" ) || trackName.startsWith( "GSM" )
                        || trackName.startsWith( "IMA" ) || trackName.startsWith( "msadpcm" )
                        || trackName.startsWith( "PCM" ) ) {// do nothing... skip to next index
                } else {
                    index = i;
                    break;
                }
            }
        }

        // System.err.println("using index: " + index);
        for ( int i = 0; i < tcs.length; i++ ) {// System.err.println(i + " " + tcs[i].getFormat().toString());
        }

        if ( compressionType == TRANSCODE_RGB ) {
            encodeStr += "uncompressed RGB";
            tcs[index].setFormat( new VideoFormat( VideoFormat.RGB ) );
        } else if ( compressionType == TRANSCODE_QT ) {
            encodeStr += "Quicktime video";
        } else if ( compressionType == TRANSCODE_MJPG ) {
            encodeStr += "Motion JPEG";
            tcs[index].setFormat( new VideoFormat( VideoFormat.MJPG ) );
        } else if ( compressionType == TRANSCODE_IV32 ) {
            encodeStr += "IR32";
            tcs[index].setFormat( new VideoFormat( "IV32" ) );
        } else if ( compressionType == TRANSCODE_IV41 ) {
            encodeStr += "IR41";
            tcs[index].setFormat( new VideoFormat( "IV41" ) );
        } else if ( compressionType == TRANSCODE_IV50 ) {
            encodeStr += "Indeo Video 5";
            tcs[index].setFormat( new VideoFormat( "IV50" ) );
        } else if ( compressionType == TRANSCODE_MP42 ) {
            encodeStr += "Mpeg-4 version 2";
            tcs[index].setFormat( new VideoFormat( "MP42" ) );
        } else if ( compressionType == TRANSCODE_MPG4 ) {
            encodeStr += "Mpeg-4 version 1";
            tcs[index].setFormat( new VideoFormat( "MPG4" ) );
        } else if ( compressionType == TRANSCODE_DIVX ) {
            encodeStr += "DIVX";
            tcs[index].setFormat( new VideoFormat( "DIVX" ) );
        } else if ( compressionType == TRANSCODE_DX50 ) {
            encodeStr += "DX50";
            tcs[0].setFormat( new VideoFormat( "DX50" ) );
        } else if ( compressionType == TRANSCODE_CVID ) {
            encodeStr += "CVID";
            tcs[index].setFormat( new VideoFormat( "cvid" ) );
        }

        // if .mov (quicktime)... dont need to set videoformat

        p.realize();
        if ( !waitForState( p, p.Realized ) ) {
            MipavUtil.displayError( "Failed to realize the processor." );
            return;
        }

        // Set the JPEG quality to .75.
        setJPEGQuality( p, quality );

        // Now, we'll need to create a DataSink.
        DataSink dsink;

        if ( ( dsink = createDataSink( p, oml ) ) == null ) {
            MipavUtil.displayError( "Failed to create a DataSink for the given output MediaLocator: " + oml );
            return;
        }

        dsink.addDataSinkListener( this );
        fileDone = false;

        int maxVal = (int) p.getDuration().getSeconds();

        // OK, we can now start the actual transcoding.

        ViewJProgressBar progressBar = null;

        try {
            // Wait for EndOfStream event.
            progressBar = new ViewJProgressBar( "Transcoding " + outputFile.getName(), encodeStr, 0, 100, false, null,
                    null );
            progressBar.updateValueImmed( 0 );
            progressBar.setLocation( (int) Toolkit.getDefaultToolkit().getScreenSize().getWidth() / 2, 50 );
            progressBar.setVisible( isProgressBarVisible() );

            p.start();
            dsink.start();
        } catch ( IOException e ) {
            MipavUtil.displayError( "IO error during transcoding" );
            return;
        }

        waitForFileDone( progressBar, maxVal );

        // Cleanup.
        try {
            dsink.close();
        } catch ( Exception e ) {}

        p.removeControllerListener( this );
        p.close();
        p = null;
        dsink = null;
        iml = null;
        oml = null;
        progressBar.setVisible( false );
        this.completed = true;
    }

    /**
     * Controller Listener.
     */
    public void controllerUpdate( ControllerEvent evt ) {

        if ( evt instanceof ConfigureCompleteEvent || evt instanceof RealizeCompleteEvent
                || evt instanceof PrefetchCompleteEvent ) {
            synchronized ( waitSync ) {
                stateTransitionOK = true;
                waitSync.notifyAll();
            }
        } else if ( evt instanceof ResourceUnavailableEvent ) {
            synchronized ( waitSync ) {
                stateTransitionOK = false;
                waitSync.notifyAll();
            }
        } else if ( evt instanceof EndOfMediaEvent ) {
            evt.getSourceController().close();
        } else if ( evt instanceof MediaTimeSetEvent ) {
            System.err.println( "- mediaTime set: " + ( (MediaTimeSetEvent) evt ).getMediaTime().getSeconds() );
        } else if ( evt instanceof StopAtTimeEvent ) {
            System.err.println( "- stop at time: " + ( (StopAtTimeEvent) evt ).getMediaTime().getSeconds() );
            evt.getSourceController().close();
        }
    }

    /**
     * Create the DataSink.
     */
    DataSink createDataSink( Processor p, MediaLocator outML ) {
        DataSource ds;

        if ( ( ds = p.getDataOutput() ) == null ) {
            MipavUtil.displayError( "Something is really wrong: the processor does not have an output DataSource" );
            return null;
        }
        DataSink dsink;

        try {
            // System.err.println("- create DataSink for: " + outML);
            dsink = Manager.createDataSink( ds, outML );
            dsink.open();
        } catch ( Exception e ) {
            MipavUtil.displayError( "Cannot create the DataSink: " + e );
            return null;
        }
        return dsink;
    }

    /**
     * Event handler for the file writer.
     */
    public void dataSinkUpdate( DataSinkEvent evt ) {

        if ( evt instanceof EndOfStreamEvent ) {
            synchronized ( waitFileSync ) {
                fileDone = true;
                waitFileSync.notifyAll();
            }
        } else if ( evt instanceof DataSinkErrorEvent ) {
            synchronized ( waitFileSync ) {
                fileDone = true;
                fileSuccess = false;
                waitFileSync.notifyAll();
            }
        }
    }

    public void finalize() {}

    /**
     * Setting the encoding quality to the specified value on the JPEG encoder.
     * 0.5 is a good default.
     */
    void setJPEGQuality( Player p, float val ) {
        Control[] cs = p.getControls();
        QualityControl qc = null;
        VideoFormat jpegFmt = new VideoFormat( VideoFormat.JPEG );

        // Loop through the controls to find the Quality control for
        // the JPEG encoder.
        for ( int i = 0; i < cs.length; i++ ) {

            if ( cs[i] instanceof QualityControl && cs[i] instanceof Owned ) {
                Object owner = ( (Owned) cs[i] ).getOwner();

                // Check to see if the owner is a Codec.
                // Then check for the output format.
                if ( owner instanceof Codec ) {
                    Format[] fmts = ( (Codec) owner ).getSupportedOutputFormats( null );

                    for ( int j = 0; j < fmts.length; j++ ) {
                        if ( fmts[j].matches( jpegFmt ) ) {
                            qc = (QualityControl) cs[i];
                            qc.setQuality( val );
                            Preferences.debug( "- Set quality to " + val + " on " + qc + "\n" );
                            break;
                        }
                    }
                }
                if ( qc != null ) {
                    break;
                }
            }
        }
    }

    /**
     * Block until file writing is done.
     */
    boolean waitForFileDone( ViewJProgressBar bar, int maxTime ) {
        synchronized ( waitFileSync ) {
            try {
                while ( !fileDone ) {
                    waitFileSync.wait( 200 );
                    bar.updateValueImmed( 100 * ( (int) p.getMediaTime().getSeconds() + 1 ) / maxTime );

                }
            } catch ( Exception e ) {}
        }
        bar.setVisible( false );
        return fileSuccess;
    }

    /**
     * Block until the processor has transitioned to the given state.
     * Return false if the transition failed.
     */
    boolean waitForState( Processor p, int state ) {
        synchronized ( waitSync ) {
            try {
                while ( p.getState() < state && stateTransitionOK ) {
                    waitSync.wait();
                }
            } catch ( Exception e ) {}
        }
        return stateTransitionOK;
    }

}
