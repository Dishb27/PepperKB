// Footer.jsx
import React from "react";
import Link from "next/link";
import styles from "../styles/footer.module.css";

const Footer = () => {
  const currentYear = new Date().getFullYear();

  const linkSections = [
    {
      title: "Quick Links",
      links: [
        { label: "Home", href: "/" },
        { label: "Gene Families", href: "/gene_families" },
        { label: "SNP Markers", href: "/snp_markers" },
        { label: "Gallery", href: "/gallery" },
      ],
    },
    {
      title: "Tools",
      links: [
        { label: "eFP Browser", href: "/efp_browser" },
        { label: "BLAST", href: "/blast" },
        { label: "JBrowse2", href: "/jbrowse2" },
        { label: "GeneViz", href: "/genetool" },
      ],
    },
    {
      title: "Help",
      links: [
        { label: "Manual", href: "/manual" },
        { label: "References", href: "/references" },
        { label: "Contact Us", href: "/contact" },
      ],
    },
  ];

  return (
    <footer className={styles.footer}>
      <div className={styles.footerContainer}>
        <div className={styles.footerContent}>
          {linkSections.map((section, index) => (
            <div key={index} className={styles.footerSection}>
              <h3 className={styles.footerTitle}>{section.title}</h3>
              <ul className={styles.footerLinks}>
                {section.links.map((link, linkIndex) => (
                  <li key={linkIndex}>
                    <Link href={link.href}>{link.label}</Link>
                  </li>
                ))}
              </ul>
            </div>
          ))}
        </div>

        <div className={styles.footerBottom}>
          {/* Uncomment this if the logo is needed */}
          {/* <div className={styles.footerLogo}>
            <img src="/images/logo.png" alt="PepperKB Logo" />
          </div> */}
          <div className={styles.footerInfo}>
            <p>Black Pepper Knowledgebase (PepperKB)</p>
            <p>© {currentYear} All rights reserved.</p>
          </div>
        </div>
      </div>
    </footer>
  );
};

export default Footer;
