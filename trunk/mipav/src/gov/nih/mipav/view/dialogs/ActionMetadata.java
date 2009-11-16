package gov.nih.mipav.view.dialogs;


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

    public String toString();
}
