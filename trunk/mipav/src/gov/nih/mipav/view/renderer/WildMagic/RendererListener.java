package gov.nih.mipav.view.renderer.WildMagic;

public interface RendererListener {

    void rendererConfigured(VolumeTriPlanarRenderBase renderer);
    void setActiveRenderer(VolumeTriPlanarRenderBase renderer);
}
