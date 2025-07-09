import React, { useState, useEffect, useRef } from "react";
import Head from "next/head";
import styles from "../styles/pepperExp.module.css";
import Header from "../components/header";
import Footer from "../components/footer";

function PepperExpPage() {
  const [loadingState, setLoadingState] = useState({
    isLoading: true,
    iframeError: false,
  });
  const [isMobile, setIsMobile] = useState(false);
  const iframeRef = useRef(null);

  useEffect(() => {
    const checkMobile = () => {
      setIsMobile(window.innerWidth <= 768);
    };

    checkMobile();
    window.addEventListener("resize", checkMobile);

    return () => window.removeEventListener("resize", checkMobile);
  }, []);

  const handleIframeLoad = () => {
    setLoadingState((prev) => ({
      ...prev,
      isLoading: false,
    }));

    // Try to communicate with the iframe about viewport size
    if (iframeRef.current) {
      try {
        // Send viewport information to the Shiny app
        const message = {
          type: "viewport-update",
          width: window.innerWidth,
          height: window.innerHeight,
          isMobile: window.innerWidth <= 768,
        };

        iframeRef.current.contentWindow?.postMessage(message, "*");
      } catch (e) {
        console.log("Could not communicate with iframe (CORS restriction)", e);
      }
    }
  };

  const handleIframeError = () => {
    setLoadingState({
      isLoading: false,
      iframeError: true,
    });
  };

  const openInNewTab = () => {
    window.open("https://dish2711.shinyapps.io/ExpressionHeatmap/", "_blank");
  };

  return (
    <>
      <Head>
        <title>Expression Heatmap - PepperExp</title>
        <meta
          name="description"
          content="Comprehensive pepper gene expression heatmap visualization"
        />
        <link
          rel="stylesheet"
          href="https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.0.0-beta3/css/all.min.css"
        />
        <meta name="viewport" content="width=device-width, initial-scale=1.0" />
      </Head>

      <Header />

      <main className={styles.appContainer}>
        <section className={styles.heroSection}>
          <div className={styles.heroContent}>
            <div className={styles.heroTextContainer}>
              <h1 className={styles.appTitle}>PepperExp</h1>
              <p className={styles.appDescription}>
                Visualize gene expression data through interactive heatmaps
              </p>
            </div>
          </div>

          {/* Mobile warning and external link */}
          {isMobile && (
            <div className={styles.mobileNotice}>
              <p className={styles.noticeText}>
                <i className="fas fa-mobile-alt"></i>
                For the best mobile experience, we recommend opening the app in
                a new tab
              </p>
              <button className={styles.openExternalBtn} onClick={openInNewTab}>
                <i className="fas fa-external-link-alt"></i>
                Open in New Tab
              </button>
            </div>
          )}
        </section>

        <section className={styles.contentContainer}>
          <div className={styles.card}>
            <div className={styles.iframeContainer}>
              {loadingState.isLoading && (
                <div className={styles.loadingOverlay}>
                  <div className={styles.spinner}></div>
                  <p className={styles.loadingText}>
                    Loading Expression Heatmap...
                  </p>
                </div>
              )}

              {loadingState.iframeError && (
                <div className={styles.errorOverlay}>
                  <i
                    className="fas fa-exclamation-triangle"
                    aria-hidden="true"
                  ></i>
                  <p>Unable to load the Expression Heatmap</p>
                  <a
                    href="https://dish2711.shinyapps.io/ExpressionHeatmap/"
                    target="_blank"
                    rel="noopener noreferrer"
                    className={styles.externalLink}
                  >
                    Open in New Tab
                  </a>
                </div>
              )}

              <iframe
                ref={iframeRef}
                src="https://dish2711.shinyapps.io/ExpressionHeatmap/"
                width="100%"
                height="800px"
                frameBorder="0"
                title="Pepper Expression Heatmap"
                aria-label="Interactive gene expression heatmap"
                onLoad={handleIframeLoad}
                onError={handleIframeError}
                style={{
                  display: loadingState.isLoading ? "none" : "block",
                  minHeight: isMobile ? "600px" : "800px",
                }}
                // Enable scrolling on mobile
                scrolling={isMobile ? "yes" : "auto"}
                // Add sandbox permissions for better interaction
                sandbox="allow-same-origin allow-scripts allow-forms allow-popups allow-pointer-lock"
              />
            </div>

            {/* Alternative mobile view */}
            {isMobile && (
              <div className={styles.mobileAlternative}>
                <p className={styles.alternativeText}>
                  Having trouble with the embedded app?
                  <button className={styles.linkButton} onClick={openInNewTab}>
                    Click here to open it directly
                  </button>
                </p>
              </div>
            )}
          </div>
        </section>
      </main>

      <Footer />
    </>
  );
}

export default PepperExpPage;
