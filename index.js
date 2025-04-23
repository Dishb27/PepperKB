import React, { useState, useEffect } from "react";
import Head from "next/head";
import Link from "next/link";
import Image from "next/image";
import { useRouter } from "next/router";
import {
  Layers,
  Search,
  Globe,
  DollarSign,
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
  const [isScrolled, setIsScrolled] = useState(false);
  const [activeTab, setActiveTab] = useState(0);
  const router = useRouter();

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
      src: "/images/pp_efp.svg",
      alt: "eFP Browser",
      label: "eFP Browser",
      description:
        "Explore gene expression data interactively across different tissues.",
      link: "/efp_browser",
      icon: Leaf,
      color: "#2ecc71",
    },
    {
      src: "/images/blast.svg",
      alt: "BLAST",
      label: "BLAST",
      description:
        "Align your sequences against the black pepper genome and proteome using the BLAST tool.",
      link: "/blast",
      icon: DollarSign,
      color: "#3498db",
    },
    {
      src: "/images/jbrowse.svg",
      alt: "JBrowse2",
      label: "JBrowse2",
      description:
        "Visualize genome annotations with the feature-rich JBrowse2 platform.",
      link: "/jbrowse2",
      icon: Globe,
      color: "#9b59b6",
    },
    {
      src: "/images/ppExp.svg",
      alt: "Expression Heatmap",
      label: "PepperExp",
      description: "Visualize gene expression data using interactive heatmaps.",
      link: "/pepperExp",
      icon: Utensils,
      color: "#e74c3c",
    },
    {
      src: "/images/snp_final.svg",
      alt: "SNP Marker Search",
      label: "SNP Marker Search",
      description: "Search for markers using chromosome and position range.",
      link: "/snp_markers",
      icon: Search,
      color: "#f39c12",
    },
    {
      src: "/images/geneviz1.svg",
      alt: "GeneViz",
      label: "GeneViz",
      description:
        "Visualize the structure of your gene of interest with ease.",
      link: "/genetool",
      icon: Search,
      color: "#f39c12",
    },
    {
      src: "/images/goPep.svg",
      alt: "GO-Pep",
      label: "GO-Pep",
      description:
        "Analyze the Gene Ontology (GO) terms of your genes of interest.",
      link: "/peppergo",
      icon: Search,
      color: "#f39c12",
    },
    {
      src: "/images/pepClust.svg",
      alt: "PepperClust",
      label: "PepperClust",
      description:
        "Cluster and compare gene expression patterns in black pepper.",
      link: "/pepperClust",
      icon: Search,
      color: "#f39c12",
    },
  ];

  useEffect(() => {
    window.addEventListener("scroll", handleScroll);
    return () => {
      window.removeEventListener("scroll", handleScroll);
    };
  }, []);

  return (
    <>
      <Head>
        <title>PepperKB - Black Pepper Knowledgebase</title>
        <meta name="viewport" content="width=device-width, initial-scale=1.0" />
      </Head>

      <Header />

      <section id="home" className="banner">
        <div className="banner-content">
          <h1>BLACK PEPPER KNOWLEDGEBASE</h1>
          <h1>(BlackPepKB)</h1>
          <p>A web resource for black pepper translational genomics</p>
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
                <span className="highlight">PepperKB</span> is a dedicated
                platform for black pepper (
                <span className="highlight-italic">
                  <strong>Piper nigrum</strong>{" "}
                </span>
                <span className="highlight">L.</span>), the world's most valued
                spice crop.
              </p>
              <p>
                It offers curated data on gene families and SNP markers, along
                with tools to support your research.
              </p>
              <p>
                Explore black pepper's genetics, analyze sequences, and learn
                more about its unique biology, all in one place.
              </p>
            </div>
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
          <h2 className="section-title">Global Production of Black Pepper</h2>
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
//$env:NODE_OPTIONS="--max-old-space-size=8192"; npm run dev
