import React, { useState, useEffect } from "react";
import Head from "next/head";
import Slider from "react-slick";
import "slick-carousel/slick/slick.css";
import "slick-carousel/slick/slick-theme.css";
import styles from "../styles/gallery.module.css";
import { Camera, X, ZoomIn } from "lucide-react";
import Header from "../components/header";
import Footer from "../components/footer";

const GalleryPage = () => {
  const [selectedImage, setSelectedImage] = useState(null);
  const [isLoading, setIsLoading] = useState(true);
  const [showCollection, setShowCollection] = useState(false);
  const [imageErrors, setImageErrors] = useState({});

  useEffect(() => {
    setTimeout(() => setIsLoading(false), 1000);
  }, []);

  // Handle image loading errors
  const handleImageError = (src) => {
    setImageErrors((prev) => ({
      ...prev,
      [src]: true,
    }));
  };

  const featuredImages = [
    {
      src: "https://res.cloudinary.com/dsjtalfn9/image/upload/front2_ogakzq.jpg",
      featured: true,
    },
    {
      src: "https://res.cloudinary.com/dsjtalfn9/image/upload/front1_bzy4ae.jpg",
      featured: true,
    },
    {
      src: "https://res.cloudinary.com/dsjtalfn9/image/upload/front3_opisx1.jpg",
      featured: true,
    },
    {
      src: "https://res.cloudinary.com/dsjtalfn9/image/upload/front4_zxpchx.jpg",
      featured: true,
    },
  ];

  const galleryImages = [
    {
      src: "https://res.cloudinary.com/dsjtalfn9/image/upload/dingirala_hfz23s.jpg",
      title: "DingiRala Variety",
      size: "large",
      details: {
        parentage: "Parentage: Panniyut-1 X GK 49",
        spikeLength: "Spike length: 12 cm",
        fillingPercent: "Filling %: 80",
        yield: "Average Yield: 2245 g/vine/year (dry)",
        conversionRatio: "Conversion Ratio: 3.27:1",
        oleoresinContent: "Oleoresin Content: 12.9%",
        oilContent: "Oil (Dry Basis): 2.8%",
        piperinContent: "Piperin (Dry Basis): 5.6%",
        bulkDensity: "Bulk Density: 613.43 g/l",
        seedWeight: "1000 seeds weight (Dry Basis): 63.44 g",
      },
    },
    {
      src: "https://res.cloudinary.com/dsjtalfn9/image/upload/bootaawe_jlujoi.jpg",
      title: "BootaweRala Variety",
      size: "medium",
      details: {
        parentage: "Parentage: Panniyut-1 X DM-7",
        spikeLength: "Spike length: 14 cm",
        fillingPercent: "Filling %: 80",
        yield: "Average Yield: 2724.2 g/vine/year (dry)",
        conversionRatio: "Conversion Ratio: 3.27:1",
        oleoresinContent: "Oleoresin Content: 12.9%",
        oilContent: "Oil (Dry Basis): 3.1%",
        piperinContent: "Piperin (Dry Basis): 6.3%",
        bulkDensity: "Bulk Density: 545.88 g/l",
        seedWeight: "1000 seeds weight (Dry Basis): 47.19 g",
      },
    },
    {
      src: "https://res.cloudinary.com/dsjtalfn9/image/upload/kohu_zztlvn.jpg",
      title: "KohukumbureRala Variety",
      size: "large",
      details: {
        parentage: "Parentage: MW-21 X Panniyut-1",
        spikeLength: "Spike length: 12 cm",
        fillingPercent: "Filling %: 80",
        yield: "Average Yield: 2340.7 g/vine/year (dry)",
        conversionRatio: "Conversion Ratio: 2.46:1",
        oleoresinContent: "Oleoresin Content: 15.4%",
        oilContent: "Oil (Dry Basis): 3.6%",
        piperinContent: "Piperin (Dry Basis): 6.0%",
        bulkDensity: "Bulk Density: 571.86 g/l",
        seedWeight: "1000 seeds weight (Dry Basis): 53.31 g",
      },
    },
    {
      src: "https://res.cloudinary.com/dsjtalfn9/image/upload/fruit1_amc5gq.jpg",
      size: "medium",
    },
    {
      src: "https://res.cloudinary.com/dsjtalfn9/image/upload/fruit2_h8cflc.jpg",
      size: "medium",
    },
    {
      src: "https://res.cloudinary.com/dsjtalfn9/image/upload/leaf_oxbxw4.jpg",
      size: "medium",
    },
    {
      src: "https://res.cloudinary.com/dsjtalfn9/image/upload/flower_hhqcoo.jpg",
      size: "medium",
    },
    {
      src: "https://res.cloudinary.com/dsjtalfn9/image/upload/fruit4_iar60w.jpg",
      size: "medium",
    },
    {
      src: "https://res.cloudinary.com/dsjtalfn9/image/upload/flower2_m1hv97.jpg",
      size: "medium",
    },
    {
      src: "https://res.cloudinary.com/dsjtalfn9/image/upload/fruit5_qzoeuh.jpg",
      size: "medium",
    },
    {
      src: "https://res.cloudinary.com/dsjtalfn9/image/upload/plant_ngb1rc.jpg",
      size: "medium",
    },
    {
      src: "https://res.cloudinary.com/dsjtalfn9/image/upload/fruit6_veq2hf.jpg",
      size: "medium",
    },
  ];

  const sliderSettings = {
    dots: true,
    infinite: true,
    speed: 1000,
    slidesToShow: 1,
    slidesToScroll: 1,
    autoplay: true,
    autoplaySpeed: 5000,
    fade: true,
    pauseOnHover: true,
    arrows: true,
  };

  useEffect(() => {
    if (showCollection) {
      const collectionSection = document.getElementById("collection");
      if (collectionSection) {
        collectionSection.scrollIntoView({
          behavior: "smooth",
          block: "start",
        });
      }
    }
  }, [showCollection]);

  const scrollToGallery = () => {
    const collectionSection = document.getElementById("collection");
    if (collectionSection) {
      collectionSection.scrollIntoView({
        behavior: "smooth",
        block: "start",
      });
    }
  };

  const handleImageClick = (image) => {
    console.log("Image clicked:", image);
    setSelectedImage(image);
  };

  return (
    <>
      <Head>
        <title>Gallery - Black Pepper Knowledgebase</title>
        <link
          rel="stylesheet"
          href="https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.0.0-beta3/css/all.min.css"
        />
      </Head>

      <Header />

      <div className={styles.container}>
        {isLoading && (
          <div className={styles.loader}>
            <div className={styles.spinner}></div>
          </div>
        )}

        <main className={styles.main}>
          <section className={styles.heroSection}>
            <Slider {...sliderSettings}>
              {featuredImages.map((image, index) => (
                <div key={index} className={styles.heroSlide}>
                  <div className={styles.imageContainer}>
                    <img
                      src={image.src}
                      alt={image.title}
                      className={styles.heroImage}
                      onError={() => handleImageError(image.src)}
                      style={{
                        width: "100%",
                        height: "100%",
                        objectFit: "cover",
                      }}
                    />
                  </div>
                  <div className={styles.heroContent}>
                    <h1>{image.title}</h1>
                    <p>{image.description}</p>
                    <button
                      className={styles.exploreButton}
                      onClick={() => {
                        setShowCollection(true);
                        scrollToGallery();
                      }}
                      aria-label="Explore image collection"
                    >
                      <Camera className={styles.buttonIcon} />
                      Explore Collection
                    </button>
                  </div>
                </div>
              ))}
            </Slider>
          </section>

          {showCollection && (
            <section id="collection" className={styles.gallerySection}>
              <div className={styles.sectionHeader}>
                <h2>Gallery</h2>
              </div>

              <div className={styles.masonryGrid}>
                {galleryImages.map((image, index) => (
                  <div
                    key={index}
                    className={`${styles.masonryItem} ${styles[image.size]}`}
                    onClick={() => handleImageClick(image)}
                    role="button"
                    tabIndex={0}
                    onKeyDown={(e) => {
                      if (e.key === "Enter" || e.key === " ") {
                        handleImageClick(image);
                      }
                    }}
                    aria-label={`View details for ${image.title}`}
                  >
                    <div className={styles.imageWrapper}>
                      {!imageErrors[image.src] ? (
                        <img
                          src={image.src}
                          alt={image.title}
                          className={styles.masonryImage}
                          onError={() => handleImageError(image.src)}
                          loading="lazy"
                          style={{
                            width: "100%",
                            height: "100%",
                            objectFit: "cover",
                          }}
                        />
                      ) : (
                        <div className={styles.imagePlaceholder}>
                          <Camera size={48} />
                          <p>Image unavailable</p>
                        </div>
                      )}
                      <div className={styles.imageOverlay}>
                        <div className={styles.overlayContent}>
                          <h4>{image.title}</h4>
                          <p>{image.description}</p>
                          <div className={styles.overlayIcons}>
                            <ZoomIn className={styles.icon} />
                          </div>
                        </div>
                      </div>
                    </div>
                  </div>
                ))}
              </div>
            </section>
          )}

          {selectedImage && (
            <div
              className={styles.lightbox}
              onClick={() => setSelectedImage(null)}
              role="dialog"
              aria-modal="true"
              aria-labelledby="lightbox-title"
            >
              {selectedImage.details ? (
                <div
                  className={styles.lightboxContent}
                  onClick={(e) => e.stopPropagation()}
                >
                  <button
                    className={styles.closeButton}
                    onClick={() => setSelectedImage(null)}
                    aria-label="Close lightbox"
                  >
                    <X />
                  </button>
                  <div className={styles.lightboxImage}>
                    <img
                      src={selectedImage.src}
                      alt={selectedImage.title}
                      style={{
                        width: "100%",
                        height: "100%",
                        objectFit: "contain",
                      }}
                    />
                  </div>
                  <div className={styles.lightboxInfo}>
                    <div className={styles.lightboxHeader}>
                      <h3 id="lightbox-title">{selectedImage.title}</h3>
                    </div>
                    {/* <p className={styles.lightboxDescription}>
                      {selectedImage.description}
                    </p> */}

                    <div className={styles.detailedInfo}>
                      {/* <div className={styles.detailItem}>
                        <strong>{selectedImage.details.title}</strong>
                      </div> */}
                      <div className={styles.detailItem}>
                        {selectedImage.details.parentage}
                      </div>
                      <div className={styles.detailItem}>
                        {selectedImage.details.spikeLength}
                      </div>
                      <div className={styles.detailItem}>
                        {selectedImage.details.fillingPercent}
                      </div>
                      <div className={styles.detailItem}>
                        {selectedImage.details.yield}
                      </div>
                      <div className={styles.detailItem}>
                        {selectedImage.details.conversionRatio}
                      </div>
                      <div className={styles.detailItem}>
                        {selectedImage.details.oleoresinContent}
                      </div>
                      <div className={styles.detailItem}>
                        {selectedImage.details.oilContent}
                      </div>
                      <div className={styles.detailItem}>
                        {selectedImage.details.piperinContent}
                      </div>
                      <div className={styles.detailItem}>
                        {selectedImage.details.bulkDensity}
                      </div>
                      <div className={styles.detailItem}>
                        {selectedImage.details.seedWeight}
                      </div>
                    </div>
                  </div>
                </div>
              ) : (
                <div
                  className={styles.lightboxContentFullImage}
                  onClick={(e) => e.stopPropagation()}
                >
                  <button
                    className={styles.closeButton}
                    onClick={() => setSelectedImage(null)}
                    aria-label="Close lightbox"
                  >
                    <X />
                  </button>
                  <div className={styles.fullImageContainer}>
                    <img
                      src={selectedImage.src}
                      alt={selectedImage.title}
                      style={{
                        width: "100%",
                        height: "100%",
                        objectFit: "contain",
                      }}
                    />
                    <div className={styles.fullImageLabel}>
                      <h3 id="lightbox-title">{selectedImage.title}</h3>
                      <p>{selectedImage.description}</p>
                    </div>
                  </div>
                </div>
              )}
            </div>
          )}
        </main>
      </div>
      <Footer />
    </>
  );
};

export default GalleryPage;
