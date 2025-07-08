import React, { useState, useEffect, useRef } from "react";
import Head from "next/head";
import Link from "next/link";
import Image from "next/image";
import {
  Layers,
  Search,
  Globe,
  Leaf,
  Utensils,
} from "lucide-react";
import {
  FaGlobe,
  FaSeedling,
  FaStethoscope,
  FaMortarPestle,
} from "react-icons/fa";
import Footer from "../components/footer";
import Header from "../components/header";

export default function Home() {
  const [, setIsScrolled] = useState(false);
  const [activeTab, setActiveTab] = useState(0);
  const [hasAnimated, setHasAnimated] = useState(false);
  const statsRef = useRef(null);
  //const router = useRouter();

  const handleScroll = () => {
    setIsScrolled(window.scrollY > window.innerHeight);
  };

  const handleTabChange = (index) => {
    setActiveTab(index);
    const tabContent = document.querySelector(".tab-content");
    if (tabContent) {
      if (index === 0) {
        tabContent.classList.add("classification-tab-content");
      } else {
        tabContent.classList.remove("classification-tab-content");
      }
    }
  };

  const sectionData = [
    {
      icon: Layers,
      title: "Classification",
      description: `
        <div class="taxonomy-container">
          <div class="taxonomy-item">
            <span class="taxonomy-level">Kingdom:</span>
            <span class="taxonomy-value" style="border-left-color: #1b837b;">Plantae</span>
          </div>
          <div class="taxonomy-item">
            <span class="taxonomy-level">Phylum:</span>
            <span class="taxonomy-value" style="border-left-color: #259ca4;">Streptophyta</span>
          </div>
          <div class="taxonomy-item">
            <span class="taxonomy-level">Class:</span>
            <span class="taxonomy-value" style="border-left-color: #45d860;">Equisetopsida</span>
          </div>
          <div class="taxonomy-item">
            <span class="taxonomy-level">Subclass:</span>
            <span class="taxonomy-value" style="border-left-color: #e09f3e;">Magnoliidae</span>
          </div>
          <div class="taxonomy-item">
            <span class="taxonomy-level">Order:</span>
            <span class="taxonomy-value" style="border-left-color: #9e2a2b;">Piperales</span>
          </div>
          <div class="taxonomy-item">
            <span class="taxonomy-level">Family:</span>
            <span class="taxonomy-value taxonomy-italic" style="border-left-color: #540b0e;">Piperaceae</span>
          </div>
          <div class="taxonomy-item">
            <span class="taxonomy-level">Genus:</span>
            <span class="taxonomy-value taxonomy-italic" style="border-left-color: #335c67;">Piper</span>
          </div>
          <div class="taxonomy-item">
            <span class="taxonomy-level">Species:</span>
            <span class="taxonomy-value taxonomy-italic" style="border-left-color: #003566;">Piper nigrum</span>
          </div>
        </div>
      `,
      color: "#1b837b",
      bgColor: "rgba(123, 104, 238, 0.05)",
      iconColor: "#1b837b",
    },
    {
      icon: FaGlobe,
      title: "Economic Significance",
      description:
        "Black pepper holds significant economic value globally. In India, it's cherished as \"Black Gold\". With a cultivation area of 458,731 hectares and an annual yield of 510,045 tons, black pepper's trade value reaches 2 billion US dollars.",
      color: "#259ca4",
      bgColor: "rgba(37, 156, 164, 0.05)",
      iconColor: "#259ca4",
    },
    {
      icon: FaSeedling,
      title: "Cultivation Regions",
      description:
        "Black pepper is grown in tropical and subtropical regions worldwide, including Ethiopia, Vietnam, Brazil, Indonesia, India, Sri Lanka, and more. It thrives in the humid climate of India's Western Ghats and has spread across various continents.",
      color: "#45d860",
      bgColor: "rgba(69, 216, 96, 0.05)",
      iconColor: "#45d860",
    },
    {
      icon: FaStethoscope,
      title: "Medicinal Uses",
      description:
        "Piperine, the active compound in black pepper, exhibits various health benefits. It has cytotoxic activity against tumor cells, offers analgesic and anti-inflammatory properties, and boosts digestive enzyme production, aiding in digestion.",
      color: "#C70039",
      bgColor: "rgba(199, 0, 57, 0.05)",
      iconColor: "#C70039",
    },
    {
      icon: FaMortarPestle,
      title: "Culinary & Traditional Uses",
      description:
        "Apart from being a culinary staple, black pepper is also utilized in traditional medicine and perfumery. Its unique flavor and pungency are attributed to piperine and volatile oils, making it a sought-after ingredient in diverse cuisines.",
      color: "#896508",
      bgColor: "rgba(137, 101, 8, 0.05)",
      iconColor: "#896508",
    },
  ];

  const navigationTools = [
    {
      src: "https://res.cloudinary.com/dsjtalfn9/image/upload/pp_efp_wft5ap.svg",
      alt: "eFP Browser",
      label: "eFP Browser",
      description:
        "Explore gene expression data interactively across different tissues.",
      link: "/efp_browser",
      icon: Leaf,
      color: "#2ecc71",
    },
    // {
    //   src: "https://res.cloudinary.com/dsjtalfn9/image/upload/blast_e1jhh1.svg",
    //   alt: "BLAST",
    //   label: "BLAST",
    //   description:
    //     "Align your sequences against the black pepper genome and proteome using the BLAST tool.",
    //   link: "/blast",
    //   icon: DollarSign,
    //   color: "#3498db",
    // },
    {
      src: "https://res.cloudinary.com/dsjtalfn9/image/upload/jbrowse_i4foyb.svg",
      alt: "JBrowse2",
      label: "JBrowse2",
      description:
        "Visualize genome annotations with the feature-rich JBrowse2 platform.",
      link: "/jbrowse2",
      icon: Globe,
      color: "#9b59b6",
    },
    {
      src: "https://res.cloudinary.com/dsjtalfn9/image/upload/ppExp_czqcch.svg",
      alt: "Expression Heatmap",
      label: "PepperExp",
      description: "Visualize gene expression data using interactive heatmaps.",
      link: "/pepperExp",
      icon: Utensils,
      color: "#e74c3c",
    },
    {
      src: "https://res.cloudinary.com/dsjtalfn9/image/upload/snp_final_zl0zja.svg",
      alt: "SNP Marker Search",
      label: "SNP Marker Search",
      description: "Search for markers using chromosome and position range.",
      link: "/snp_markers",
      icon: Search,
      color: "#f39c12",
    },
    {
      src: "https://res.cloudinary.com/dsjtalfn9/image/upload/geneviz1_tl6g0r.svg",
      alt: "GeneViz",
      label: "GeneViz",
      description:
        "Visualize the structure of your gene of interest with ease.",
      link: "/genetool",
      icon: Search,
      color: "#f39c12",
    },
    {
      src: "https://res.cloudinary.com/dsjtalfn9/image/upload/goPep_cok5xv.svg",
      alt: "GO-Pep",
      label: "GO-Pep",
      description:
        "Analyze the Gene Ontology (GO) terms of your genes of interest.",
      link: "/peppergo",
      icon: Search,
      color: "#f39c12",
    },
    {
      src: "https://res.cloudinary.com/dsjtalfn9/image/upload/pepClust_xhksvc.svg",
      alt: "PepperClust",
      label: "PepperClust",
      description:
        "Cluster and compare gene expression patterns in black pepper.",
      link: "/pepperClust",
      icon: Search,
      color: "#f39c12",
    },
  ];

  const stats = [
    { label: "Genes", value: 63451 },
    { label: "SNPs", value: 432418 },
    { label: "GO Terms", value: 273018 },
    { label: "TF Families", value: 62 },
  ];

  const animateCounters = () => {
    if (hasAnimated) return;

    stats.forEach((stat, index) => {
      const element = document.getElementById(`counter-${index}`);
      if (!element) return;

      let current = 0;
      const target = stat.value;
      const increment = target / 100;
      const duration = 2000; // 2 seconds
      const intervalTime = duration / 100;

      const counter = setInterval(() => {
        current += increment;
        if (current >= target) {
          element.textContent = target.toLocaleString();
          clearInterval(counter);
        } else {
          element.textContent = Math.floor(current).toLocaleString();
        }
      }, intervalTime);
    });

    setHasAnimated(true);
  };

  useEffect(() => {
    const observer = new IntersectionObserver(
      (entries) => {
        entries.forEach((entry) => {
          if (entry.isIntersecting && !hasAnimated) {
            animateCounters();
          }
        });
      },
      { threshold: 0.5 }
    );

    if (statsRef.current) {
      observer.observe(statsRef.current);
    }

    return () => {
      if (statsRef.current) {
        observer.unobserve(statsRef.current);
      }
    };
  }, [hasAnimated]);

  useEffect(() => {
    window.addEventListener("scroll", handleScroll);
    return () => {
      window.removeEventListener("scroll", handleScroll);
    };
  }, []);

  return (
    <>
      <Head>
        <title>BlackPepKB - Black Pepper Knowledgebase</title>
        <meta name="viewport" content="width=device-width, initial-scale=1.0" />
        <link
          rel="preload"
          as="image"
          href="https://res.cloudinary.com/dsjtalfn9/image/upload/q_auto,f_auto,w_800/fruit3_ngcrh2.jpg"
          media="(max-width: 768px)"
        />
        <link
          rel="preload"
          as="image"
          href="https://res.cloudinary.com/dsjtalfn9/image/upload/fruit3_ngcrh2.jpg"
          media="(min-width: 769px)"
        />
      </Head>

      <Header />

      <section id="home" className="banner">
        {/* Enhanced mobile fallback image with multiple sources */}
        <picture>
          <source
            media="(max-width: 480px)"
            srcSet="https://res.cloudinary.com/dsjtalfn9/image/upload/q_auto,f_auto,w_500,h_600,c_fill/fruit3_ngcrh2.jpg"
          />
          <source
            media="(max-width: 768px)"
            srcSet="https://res.cloudinary.com/dsjtalfn9/image/upload/q_auto,f_auto,w_800,h_700,c_fill/fruit3_ngcrh2.jpg"
          />
          <img
            src="https://res.cloudinary.com/dsjtalfn9/image/upload/q_auto,f_auto,w_800/fruit3_ngcrh2.jpg"
            alt="Black Pepper Background"
            className="banner-mobile-fallback"
            loading="eager"
            fetchPriority="high"
            decoding="async"
            onError={(e) => {
              // Fallback if Cloudinary image fails
              e.target.style.display = "none";
              e.target.parentElement.style.backgroundColor = "#1b837b";
            }}
          />
        </picture>

        <div className="banner-content">
          <h1>BLACK PEPPER KNOWLEDGEBASE</h1>
          <h1>(BlackPepKB)</h1>
          <p>
            A comprehensive web resource for black pepper translation genomics,
            providing valuable insights into gene functions, annotations and SNP
            markers
          </p>
          <Link href="#introduction" className="cta-button">
            Explore Now
          </Link>
        </div>
      </section>

      <section id="introduction" className="introduction-section">
        <div className="intro-container">
          <div className="intro-card">
            <h1 className="intro-title">
              Welcome to the Black Pepper KnowledgeBase
            </h1>
            <div className="intro-content">
              <p>
                <span className="highlight">BlackPepKB</span> is a dedicated
                platform for black pepper, the world&apos;s most valuable spice
                crop.
              </p>
              <p>
                It provides curated data on gene families and SNP markers, along
                with essential tools to support your research.
              </p>
              <p>
                Explore the black pepper genome, analyze sequences, visualize
                gene expression data, and access a wealth of integrated
                information—all in one place.
              </p>
            </div>
          </div>
        </div>
      </section>

      <section className="stats-section" ref={statsRef}>
        <div className="section-container">
          <h2 className="section-title">Database Statistics</h2>
          <div className="stats-grid">
            {stats.map((stat, index) => (
              <div className="stat-card" key={index}>
                <h3 className="stat-number" id={`counter-${index}`}>
                  0
                </h3>
                <p className="stat-label">{stat.label}</p>
              </div>
            ))}
          </div>
        </div>
      </section>

      <section className="features-section">
        <div className="section-container">
          <h2 className="section-title">Background</h2>
          <div className="features-tabs">
            <div className="tab-navigation">
              {sectionData.map((item, index) => (
                <button
                  key={index}
                  className={`tab-button ${
                    activeTab === index ? "active" : ""
                  }`}
                  onClick={() => handleTabChange(index)}
                  style={{
                    "--tab-color": item.color,
                  }}
                >
                  <item.icon className="tab-icon" />
                  <span>{item.title}</span>
                </button>
              ))}
            </div>
            <div
              className="tab-content"
              style={{
                "--content-color": sectionData[activeTab].color,
                "--content-bg": sectionData[activeTab].bgColor,
              }}
            >
              <div className="content-header">
                <div className="header-icon-container">
                  {React.createElement(sectionData[activeTab].icon, {
                    className: "header-icon",
                  })}
                </div>
                <h3 className="content-title">
                  {sectionData[activeTab].title}
                </h3>
              </div>
              <div
                className="content-description"
                dangerouslySetInnerHTML={{
                  __html: sectionData[activeTab].description,
                }}
              />
            </div>
          </div>
        </div>
      </section>

      <section id="map" className="map-section">
        <div className="section-container">
          <h2 className="section-title">Black Pepper Global Production</h2>
          <div className="map-container">
            <div className="map-overlay"></div>
            <iframe
              src="https://dish2711.shinyapps.io/MapProduction/"
              className="map-iframe"
              title="Map Production"
            ></iframe>
          </div>
        </div>
      </section>

      <section id="quick-navigation" className="tools-section">
        <div className="section-container">
          <h2 className="section-title">Tools and Databases</h2>
          <div className="tools-grid">
            {navigationTools.map((tool, index) => (
              <Link href={tool.link} key={index} className="tool-card">
                <div className="tool-image-container">
                  <Image
                    src={tool.src}
                    alt={tool.alt}
                    width={100}
                    height={100}
                    className="tool-image"
                  />
                  <div className="tool-overlay">
                    <span className="explore-text">Explore</span>
                  </div>
                </div>
                <div className="tool-content">
                  <h3 className="tool-title">{tool.label}</h3>
                  <p className="tool-description">{tool.description}</p>
                </div>
              </Link>
            ))}
          </div>
        </div>
      </section>

      <Footer />
    </>
  );
}
