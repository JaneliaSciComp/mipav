package gov.nih.mipav.view.dialogs;


import java.util.Set;


public interface ActionMetadata {
    public String getName();

    public String getLabel();

    public String getShortLabel();

    public String getDescription();

    public String getDescriptionLong();

    public String getCategory();

    public String getWebsite();

    public String getVersion();

    public String[] getAuthors();

    public String[] getAffiliation();

    public String[] getCitations();

    public String toString();

    public Set<ImageRequirements> getInputImageRequirements();

    public enum ImageRequirements {
        NDIM_2("2D image"), NDIM_3("3D image"), NDIM_4("4D image"), NDIM_5("5D image"), NDIM_6("6D image"), COLOR(
                "Color image"), GRAYSCALE("Grayscale image"), DATA_BOOLEAN("Boolean image"), DATA_BYTE("Byte image"), DATA_DOUBLE(
                "Double precision floating point image"), DATA_FLOAT("Floating point image"), DATA_INTEGER(
                "Integer image"), DATA_LONG("Long image"), DATA_SHORT("Short image"), DATA_UBYTE("Unsigned byte image"), DATA_UINTEGER(
                "Unsigned integer image"), DATA_USHORT("Unsigned short image"), DATA_COMPLEX(
                "Floating point complex image"), DATA_DCOMPLEX("Double precision floating point complex image");

        public String description;

        ImageRequirements(final String desc) {
            description = desc;
        }
    }
}
