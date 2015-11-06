package gov.nih.mipav.model.file;


import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.dialogs.*;


/**
 * File info storage container.
 */
public class FileInfoLIFF extends FileInfoBase {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    //private static final long serialVersionUID;
    private String layerString[] = null;
    short bitDepth = 0;
    double autoContrast = Double.NaN;
    double binning = Double.NaN;
    String camera = null;
    String channelArray[] = null;
    double colorization = Double.NaN;
    double cooling = Double.NaN;
    double CRIRGBFilter = Double.NaN;
    double digitalGain = Double.NaN;
    double emissionFilterChangerArray[] = null;
    int channelNumber = 0;
    double excitationArray[] = null;
    double exposureArray[] = null;
    double filterTurretArray[] = null;
    double focusPosition = Double.NaN;
    double gain = Double.NaN;
    double leicaCondenserTurret = Double.NaN;
    double leicaFilterCubeArray[] = null;
    double leicaFIMArray[] = null;
    double leicaICTurret = Double.NaN;
    double leicaMagnificationChanger = Double.NaN;
    double lightMode = Double.NaN;
    double ludlAuxWheel1 = Double.NaN;
    double ludlMainWheel1Array[] = null;
    String microscope = null;
    double microfocusPosition = Double.NaN;
    double objectiveName = Double.NaN;
    double objectivePosition = Double.NaN;
    double offset = Double.NaN;
    double sensitivityArray[] = null;
    double SutterDG4Filter = Double.NaN;
    double SutterL10Filter1Array[] = null;
    double SutterL10Filter2Array[] = null;
    double wavelengthArray[] = null;
    double xPosition = Double.NaN;
    double yPosition = Double.NaN;
    double zPosition = Double.NaN;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * File info storage constructor.
     *
     * @param  name       file name
     * @param  directory  directory
     * @param  format     file format
     */
    public FileInfoLIFF(String name, String directory, int format) {
        super(name, directory, format);
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Displays the file information.
     *
     * @param  dlog    dialog box that is written to
     * @param  matrix  transformation matrix
     */
    public void displayAboutInfo(JDialogBase dlog, TransMatrix matrix) {
        int i;
        JDialogText dialog = (JDialogText) dlog;
        displayPrimaryInfo(dialog, matrix);
        dialog.append("\n\n                Other information\n\n");
        if (layerString != null) {
            for (i = 0; i < layerString.length; i++) {
                if (layerString[i] != null) {
                    dialog.append("Layer " + (i + 1) + " = " + layerString[i].trim() + "\n");
                }
            }
        }
        
        if (bitDepth > 0) {
            dialog.append("Bit depth = " + bitDepth + "\n");
        }
        
        if (!Double.isNaN(autoContrast)) {
            dialog.append("Auto-contrast = " + autoContrast + "\n");
        }
        
        if (!Double.isNaN(binning)) {
            dialog.append("Binning = " + binning + "\n");
        }
        
        if (camera != null) {
            dialog.append("Camera = " + camera + "\n");
        }
        
        if (!Double.isNaN(colorization)) {
            dialog.append("Colorization = " + colorization + "\n");
        }
        
        if (channelArray != null) {
            dialog.append("Channels: \n");
            for (i = 0; i < channelArray.length; i++) {
                if (channelArray[i] != null) {
                    dialog.append("\t" + channelArray[i] + "\n");
                    channelNumber++;
                }
            }
        }
        
        if (!Double.isNaN(cooling)) {
            dialog.append("Cooling = " + cooling + "\n");
        }
        
        if (!Double.isNaN(digitalGain)) {
            dialog.append("Digital gain = " + digitalGain + "\n");
        }
        
        if (!Double.isNaN(CRIRGBFilter)) {
            dialog.append("CRI RGB Filter = " + CRIRGBFilter + "\n");
        }
        
        if (emissionFilterChangerArray != null) {
            for (i = 0; i < emissionFilterChangerArray.length; i++) {
                if ((!Double.isNaN(emissionFilterChangerArray[i])) && (channelArray != null) && (channelArray[i] != null)) {
                    dialog.append("Emission Filter Changer = " + emissionFilterChangerArray[i] + 
                                  " for channel = " + channelArray[i] + "\n");
                }
                else if (!Double.isNaN(emissionFilterChangerArray[i])) {
                    dialog.append("Emission Filter Changer = " + emissionFilterChangerArray[i] +
                            " for channel = " + (i + 1) + "\n");
                }
            }
        } // if (emissionFilterChangerArray != null)      
        
        if (excitationArray != null) {
            for (i = 0; i < excitationArray.length; i++) {
                if ((!Double.isNaN(excitationArray[i])) && (channelArray != null) && (channelArray[i] != null)) {
                    dialog.append("Excitation Filter Changer = " + excitationArray[i] + 
                                  " for channel = " + channelArray[i] + "\n");
                }
                else if (!Double.isNaN(excitationArray[i])) {
                    dialog.append("Excitation Filter Changer = " + excitationArray[i] + 
                            " for channel = " + (i + 1) + "\n");
                }
            }
        } // if (excitationArray != null)
        
        if (exposureArray != null) {
            for (i = 0; i < exposureArray.length; i++) {
                if ((!Double.isNaN(exposureArray[i])) && (channelArray != null) && (channelArray[i] != null)) {
                    dialog.append("Exposure = " + exposureArray[i] + 
                                  " for channel = " + channelArray[i] + "\n");
                }
                else if (!Double.isNaN(exposureArray[i])) {
                    dialog.append("Exposure = " + exposureArray[i] + 
                            " for channel = " + (i + 1) + "\n");
                }
            }
        } // if (exposureArray != null)
        
        if (filterTurretArray != null) {
            for (i = 0; i < filterTurretArray.length; i++) {
                if ((!Double.isNaN(filterTurretArray[i])) && (channelArray != null) && (channelArray[i] != null)) {
                    dialog.append("Filter Turret = " + filterTurretArray[i] + 
                                  " for channel = " + channelArray[i] + "\n");
                }
                else if (!Double.isNaN(filterTurretArray[i])) {
                    dialog.append("Filter Turret = " + filterTurretArray[i] + 
                            " for channel = " + (i + 1) + "\n");
                }
            }
        } // if (filterTurretArray != null)
        
        if (!Double.isNaN(focusPosition)) {
            dialog.append("Focus Position = " + focusPosition + "\n");
        }
        
        if (!Double.isNaN(gain)) {
            dialog.append("Gain = " + gain + "\n");
        }
        
        if (!Double.isNaN(leicaCondenserTurret)) {
            dialog.append("Leica Condenser Turret = " + leicaCondenserTurret + "\n");
        }
        
        if (leicaFilterCubeArray != null) {
            for (i = 0; i < leicaFilterCubeArray.length; i++) {
                if ((!Double.isNaN(leicaFilterCubeArray[i])) && (channelArray != null) && (channelArray[i] != null)) {
                    dialog.append("Leica Filter Cube = " + leicaFilterCubeArray[i] + 
                                  " for channel = " + channelArray[i] + "\n");
                }
                else if (!Double.isNaN(leicaFilterCubeArray[i])) {
                    dialog.append("Leica Filter Cube = " + leicaFilterCubeArray[i] + 
                            " for channel = " + (i + 1) + "\n");
                }
            }
        } // if (leicaFilterCubeArray != null)
        
        if (leicaFIMArray != null) {
            for (i = 0; i < leicaFIMArray.length; i++) {
                if ((!Double.isNaN(leicaFIMArray[i])) && (channelArray != null) && (channelArray[i] != null)) {
                    dialog.append("Leica FIM = " + leicaFIMArray[i] + 
                                  " for channel = " + channelArray[i] + "\n");
                }
                else if (!Double.isNaN(leicaFIMArray[i])) {
                    dialog.append("Leica FIM = " + leicaFIMArray[i] + "\n");
                }
            }
        } // if (leicaFIMArray != null)
        
        if (!Double.isNaN(leicaICTurret)) {
            dialog.append("Leica IC turret = " + leicaICTurret + "\n");
        }
        
        if (!Double.isNaN(leicaMagnificationChanger)) {
            dialog.append("Leica Magnification Changer = " + leicaMagnificationChanger + "\n");
        }
        
        if (!Double.isNaN(lightMode)) {
            dialog.append("Light Mode = " + lightMode + "\n");
        }
        
        if (!Double.isNaN(ludlAuxWheel1)) {
            dialog.append("Ludl Aux. Wheel 1 = " + ludlAuxWheel1 + "\n");
        }
        
        if (ludlMainWheel1Array != null) {
            for (i = 0; i < ludlMainWheel1Array.length; i++) {
                if ((!Double.isNaN(ludlMainWheel1Array[i])) && (channelArray != null) && (channelArray[i] != null)) {
                    dialog.append("Ludl Main Wheel 1 = " + ludlMainWheel1Array[i] + 
                                  " for channel = " + channelArray[i] + "\n");
                }
                else if (!Double.isNaN(ludlMainWheel1Array[i])) {
                    dialog.append("Ludl Main Wheel 1 = " + ludlMainWheel1Array[i] + 
                            " for channel = " + (i + 1) + "\n");
                }
            }
        } // if (ludlMainWheel1Array != null)
        
        if (!Double.isNaN(microfocusPosition)) {
            dialog.append("Microfocus Position = " + microfocusPosition + "\n");
        }
        
        if (microscope != null) {
            dialog.append("Microscope = " + microscope + "\n");
        }
        
        if (!Double.isNaN(objectiveName)) {
            dialog.append("Objective Name = " + objectiveName + "\n");
        }
        
        if (!Double.isNaN(objectivePosition)) {
            dialog.append("Objective Position = " + objectivePosition + "\n");
        }
        
        if (!Double.isNaN(offset)) {
            dialog.append("Offset = " + offset + "\n");
        }
        
        if (sensitivityArray != null) {
            for (i = 0; i < sensitivityArray.length; i++) {
                if ((!Double.isNaN(sensitivityArray[i])) && (channelArray != null) && (channelArray[i] != null)) {
                    dialog.append("Sensitivity = " + sensitivityArray[i] + 
                                  " for channel = " + channelArray[i] + "\n");
                }
                else if (!Double.isNaN(sensitivityArray[i])) {
                    dialog.append("Sensitivity = " + sensitivityArray[i] + 
                            " for channel = " + (i + 1) + "\n");
                }
            }
        } // if (sensitivityArray != null)
        
        if (!Double.isNaN(SutterDG4Filter)) {
            dialog.append("Sutter DG-4 Filter = " + SutterDG4Filter + "\n");
        }
        
        if (SutterL10Filter1Array != null) {
            for (i = 0; i < SutterL10Filter1Array.length; i++) {
                if ((!Double.isNaN(SutterL10Filter1Array[i])) && (channelArray != null) && (channelArray[i] != null)) {
                    dialog.append("Sutter L-10 Filter 1 = " + SutterL10Filter1Array[i] + 
                                  " for channel = " + channelArray[i] + "\n");
                }
                else if (!Double.isNaN(SutterL10Filter1Array[i])) {
                    dialog.append("Sutter L-10 Filter 1 = " + SutterL10Filter1Array[i] + 
                            " for channel = " + (i + 1) + "\n");
                }
            }
        } // if (SutterL10Filter1Array != null)
        
        if (SutterL10Filter2Array != null) {
            for (i = 0; i < SutterL10Filter2Array.length; i++) {
                if ((!Double.isNaN(SutterL10Filter2Array[i])) && (channelArray != null) && (channelArray[i] != null)) {
                    dialog.append("Sutter L-10 Filter 2 = " + SutterL10Filter2Array[i] + 
                                  " for channel = " + channelArray[i] + "\n");
                }
                else if (!Double.isNaN(SutterL10Filter2Array[i])) {
                    dialog.append("Sutter L-10 Filter 2 = " + SutterL10Filter2Array[i] + 
                            " for channel = " + (i + 1) + "\n");
                }
            }
        } // if (SutterL10Filter2Array != null)
        
        if (wavelengthArray != null) {
            for (i = 0; i < wavelengthArray.length; i++) {
                if ((!Double.isNaN(wavelengthArray[i])) && (channelArray != null) && (channelArray[i] != null)) {
                    dialog.append("Wavelength = " + wavelengthArray[i] + 
                                  " for channel = " + channelArray[i] + "\n");
                }
                else if (!Double.isNaN(wavelengthArray[i])) {
                    dialog.append("Wavelength = " + wavelengthArray[i] + 
                            " for channel = " + (i + 1) + "\n");
                }
            }
        } // if (wavelengthArray != null)
        
        if (!Double.isNaN(xPosition)) {
            dialog.append("X-Y Stage: X Position = " + xPosition + "\n");
        }
        
        if (!Double.isNaN(yPosition)) {
            dialog.append("X-Y Stage: Y Position = " + yPosition + "\n");
        }
        
        if (!Double.isNaN(zPosition)) {
            dialog.append("Z Position = " + zPosition + "\n");
        }
        
    }
    
    public void setLayerString(String layerString[]) {
        this.layerString = layerString;
    }
    
    public void setBitDepth(short bitDepth) {
        this.bitDepth = bitDepth;
    }
    
    public void setAutoContrast(double autoContrast) {
        this.autoContrast = autoContrast;
    }

    public void setBinning(double binning) {
        this.binning = binning;
    }
    
    public void setCamera(String camera) {
        this.camera = camera;
    }
    
    public void setChannelArray(String channelArray[]) {
        this.channelArray = channelArray;
    }
    
    public void setColorization(double colorization) {
        this.colorization = colorization;
    }
    
    public void setCooling(double cooling) {
        this.cooling = cooling;
    }
    
    public void setCRIRGBFilter(double CRIRGBFilter) {
        this.CRIRGBFilter = CRIRGBFilter;
    }
    
    public void setDigitalGain(double digitalGain) {
        this.digitalGain = digitalGain;
    }
    
    public void setEmissionFilterChangerArray(double emissionFilterChangerArray[]) {
        this.emissionFilterChangerArray = emissionFilterChangerArray;
    }
    
    public void setExcitationArray(double excitationArray[]) {
        this.excitationArray = excitationArray;
    }
    
    public void setExposureArray(double exposureArray[]) {
        this.exposureArray = exposureArray;
    }
    
    public void setFilterTurretArray(double filterTurretArray[]) {
        this.filterTurretArray = filterTurretArray;
    }
    
    public void setFocusPosition(double focusPosition) {
        this.focusPosition = focusPosition;
    }
    
    public void setGain(double gain) {
        this.gain = gain;
    }
    
    public void setLeicaCondenserTurret(double leicaCondenserTurret) {
        this.leicaCondenserTurret = leicaCondenserTurret;
    }
    
    public void setLeicaFilterCubeArray(double leicaFilterCubeArray[]) {
        this.leicaFilterCubeArray = leicaFilterCubeArray;
    }
    
    public void setLeicaFIMArray(double leicaFIMArray[]) {
        this.leicaFIMArray = leicaFIMArray;
    }
    
    public void setLeicaICTurret(double leicaICTurret) {
        this.leicaICTurret = leicaICTurret;
    }
    
    public void setLeicaMagnificationChanger(double leicaMagnificationChanger) {
        this.leicaMagnificationChanger = leicaMagnificationChanger;
    }
    
    public void setLightMode(double lightMode) {
        this.lightMode = lightMode;
    }
    
    public void setLudlAuxWheel1(double ludlAuxWheel1) {
        this.ludlAuxWheel1 = ludlAuxWheel1;
    }
    
    public void setLudlMainWheel1Array(double ludlMainWheel1Array[]) {
        this.ludlMainWheel1Array = ludlMainWheel1Array;
    }
    
    public void setMicrofocusPosition(double microfocusPosition) {
        this.microfocusPosition = microfocusPosition;
    }
    
    public void setMicroscope(String microscope) {
        this.microscope = microscope;
    }
    
    public void setObjectiveName(double objectiveName) {
        this.objectiveName = objectiveName;
    }
    
    public void setObjectivePosition(double objectivePosition) {
        this.objectivePosition = objectivePosition;
    }
    
    public void setOffset(double offset) {
        this.offset = offset;
    }
    
    public void setSensitivityArray(double sensitivityArray[]) {
        this.sensitivityArray = sensitivityArray;
    }
    
    public void setSutterDG4Filter(double SutterDG4Filter) {
        this.SutterDG4Filter = SutterDG4Filter;
    }
    
    public void setSutterL10Filter1Array(double SutterL10Filter1Array[]) {
        this.SutterL10Filter1Array = SutterL10Filter1Array;
    }
    
    public void setSutterL10Filter2Array(double SutterL10Filter2Array[]) {
        this.SutterL10Filter2Array = SutterL10Filter2Array;
    }
    
    public void setWavelengthArray(double wavelengthArray[]) {
        this.wavelengthArray = wavelengthArray;
    }
    
    public void setXPosition(double xPosition) {
        this.xPosition = xPosition;
    }
    
    public void setYPosition(double yPosition) {
        this.yPosition = yPosition;
    }
    
    public void setZPosition(double zPosition) {
        this.zPosition = zPosition;
    }
}
