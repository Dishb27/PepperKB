import React, { useState, useEffect } from "react";
import Head from "next/head";
import Image from "next/image";
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

  useEffect(() => {
    setTimeout(() => setIsLoading(false), 1000); // Simulate loading for smoother transition
  }, []);

  const featuredImages = [
    {
      src: "/images/front2.jpg",
      title: "",
      description: "",
      featured: true,
    },
    {
      src: "/images/front1.jpg",
      title: "",
      description: "",
      featured: true,
    },
    // {
    //   src: "/images/front5.jpg",
    //   title: "",
    //   description: "",
    //   featured: true,
    // },
    {
      src: "/images/front3.jpg",
      title: "",
      description: "",
      featured: true,
    },
    {
      src: "/images/front4.jpg",
      title: "",
      description: "",
      featured: true,
    },
  ];

  const galleryImages = [
    {
      src: "/images/dingirala.jpg",
      title: "DingiRala Variety",
      description: "",
      size: "large",
      details: {
        title: "DingiRala - New Black Pepper Hybrid",
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
      src: "/images/bootaawe.jpg",
      title: "BootaweRala Variety",
      description: "",
      size: "medium",
      details: {
        title: "BootaweRala - New Black Pepper Hybrid",
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
      src: "/images/kohu.jpg",
      title: "KohukumbureRala Variety",
      description: "",
      size: "large",
      details: {
        title: "KohukumbureRala - New Black Pepper Hybrid",
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
      src: "/images/fruit1.jpg",
      title: "",
      description: "",
      size: "medium",
    },
    {
      src: "/images/fruit2.jpg",
      title: "",
      description: "",
      size: "medium",
    },
    {
      src: "/images/leaf.jpg",
      title: "",
      description: "",
      size: "medium",
    },
    {
      src: "/images/flower.jpg",
      title: "",
      description: "",
      size: "medium",
    },
    {
      src: "/images/fruit4.jpg",
      title: "",
      description: "",
      size: "medium",
    },
    {
      src: "/images/flower2.jpg",
      title: "",
      description: "",
      size: "medium",
    },
    {
      src: "/images/fruit5.jpg",
      title: "",
      description: "",
      size: "medium",
    },
    {
      src: "/images/plant.jpg",
      title: "",
      description: "",
      size: "medium",
    },
    {
      src: "/images/fruit6.jpg",
      title: "",
      description: "",
      size: "medium",
    },
  ];

  const sliderSettings = {
    dots: false,
    infinite: true,
    speed: 1000,
    slidesToShow: 1,
    slidesToScroll: 1,
    autoplay: true,
    autoplaySpeed: 5000,
    fade: true,
    pauseOnHover: false,
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
    console.log("Image clicked:", image); // Debugging line
    setSelectedImage(image); // Set selected image with details
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

      {/* Header with Navigation */}
      <Header />

      {/* Gallery Page Content */}
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
                  <Image
                    src={image.src}
                    alt={image.title}
                    layout="fill"
                    objectFit="cover"
                    priority
                    className={styles.heroImage}
                  />
                  <div className={styles.heroContent}>
                    <h1>{image.title}</h1>
                    <p>{image.description}</p>
                    <button
                      className={styles.exploreButton}
                      onClick={() => {
                        setShowCollection(true);
                        scrollToGallery();
                      }}
                    >
                      <Camera className={styles.buttonIcon} />
                      Explore Collection
                    </button>
                  </div>
                </div>
              ))}
            </Slider>
          </section>

          {/* Conditionally rendered gallery section */}
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
                    onClick={() => handleImageClick(image)} // Handle image click
                  >
                    <div className={styles.imageWrapper}>
                      <Image
                        src={image.src}
                        alt={image.title}
                        layout="fill"
                        objectFit="cover"
                        className={styles.masonryImage}
                      />
                      <div className={styles.imageOverlay}>
                        <div className={styles.overlayContent}>
                          <h4>{image.title}</h4>
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

          {/* Lightbox for selected image - Different layouts based on whether image has details */}
          {selectedImage && (
            <div
              className={styles.lightbox}
              onClick={() => setSelectedImage(null)}
            >
              {selectedImage.details ? (
                // Split view for images with details
                <div
                  className={styles.lightboxContent}
                  onClick={(e) => e.stopPropagation()}
                >
                  <button
                    className={styles.closeButton}
                    onClick={() => setSelectedImage(null)}
                  >
                    <X />
                  </button>
                  <div className={styles.lightboxImage}>
                    <Image
                      src={selectedImage.src}
                      alt={selectedImage.title}
                      layout="fill"
                      objectFit="contain"
                    />
                  </div>
                  <div className={styles.lightboxInfo}>
                    <div className={styles.lightboxHeader}>
                      <h3>{selectedImage.title}</h3>
                    </div>
                    <p className={styles.lightboxDescription}>
                      {selectedImage.description}
                    </p>

                    {/* Show details for selected image */}
                    <div className={styles.detailedInfo}>
                      <p>
                        <strong>{selectedImage.details.title}</strong>
                      </p>
                      <p>{selectedImage.details.parentage}</p>
                      <p>{selectedImage.details.spikeLength}</p>
                      <p>{selectedImage.details.fillingPercent}</p>
                      <p>{selectedImage.details.yield}</p>
                      <p>{selectedImage.details.conversionRatio}</p>
                      <p>{selectedImage.details.oleoresinContent}</p>
                      <p>{selectedImage.details.oilContent}</p>
                      <p>{selectedImage.details.piperinContent}</p>
                      <p>{selectedImage.details.bulkDensity}</p>
                      <p>{selectedImage.details.seedWeight}</p>
                    </div>
                  </div>
                </div>
              ) : (
                // Full image view for images without details
                <div
                  className={styles.lightboxContentFullImage}
                  onClick={(e) => e.stopPropagation()}
                >
                  <button
                    className={styles.closeButton}
                    onClick={() => setSelectedImage(null)}
                  >
                    <X />
                  </button>
                  <div className={styles.fullImageContainer}>
                    <Image
                      src={selectedImage.src}
                      alt={selectedImage.title}
                      layout="fill"
                      objectFit="contain"
                    />
                    <div className={styles.fullImageLabel}>
                      <h3>{selectedImage.title}</h3>
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
