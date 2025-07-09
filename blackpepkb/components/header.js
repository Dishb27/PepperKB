import React, { useState, useEffect, useRef } from "react";
import Link from "next/link";
import { Search, X } from "lucide-react";
import styles from "../styles/header.module.css";
import { useRouter } from "next/router";

const Header = () => {
  const router = useRouter();
  const [isMenuOpen, setIsMenuOpen] = useState(false);
  const [openDropdown, setOpenDropdown] = useState(null);
  const [geneId, setGeneId] = useState("");
  const [scrolled, setScrolled] = useState(false);
  const [isSearchFocused, setIsSearchFocused] = useState(false);
  const [isMobile, setIsMobile] = useState(false);

  // Create refs for each dropdown
  const dropdownRefs = useRef([]);
  const searchInputRef = useRef(null);

  useEffect(() => {
    const checkIfMobile = () => {
      setIsMobile(window.innerWidth <= 900);
    };

    // Initial check
    checkIfMobile();

    // Add event listener for window resize
    window.addEventListener("resize", checkIfMobile);

    // Cleanup
    return () => window.removeEventListener("resize", checkIfMobile);
  }, []);

  // Handle scroll event to change header appearance
  useEffect(() => {
    const handleScroll = () => {
      setScrolled(window.scrollY > 20);
    };

    window.addEventListener("scroll", handleScroll);
    return () => {
      window.removeEventListener("scroll", handleScroll);
    };
  }, []);

  const toggleMenu = () => {
    setIsMenuOpen(!isMenuOpen);
    // Close any open dropdown when toggling menu
    setOpenDropdown(null);
  };

  // Modified to handle both click and hover
  const handleDropdownInteraction = (index, e) => {
    if (e) {
      e.preventDefault();
      e.stopPropagation(); // Prevent event from bubbling up
    }
    setOpenDropdown(openDropdown === index ? null : index);
  };

  const handleSearch = (e) => {
    e.preventDefault();
    if (geneId.trim()) {
      router.push(`/summary?geneID=${geneId}`);
      // Close mobile menu after search on mobile
      if (isMobile && isMenuOpen) {
        setIsMenuOpen(false);
      }
    }
  };

  const clearSearch = () => {
    setGeneId("");
    if (searchInputRef.current) {
      searchInputRef.current.focus();
    }
  };

  // Close dropdown when clicking outside
  useEffect(() => {
    const handleClickOutside = (event) => {
      // Only process if we have an open dropdown
      if (openDropdown !== null) {
        // Check if the click was inside the active dropdown
        const clickedInsideActiveDropdown =
          dropdownRefs.current[openDropdown] &&
          dropdownRefs.current[openDropdown].contains(event.target);

        if (!clickedInsideActiveDropdown) {
          setOpenDropdown(null);
        }
      }
    };

    document.addEventListener("mousedown", handleClickOutside);
    return () => {
      document.removeEventListener("mousedown", handleClickOutside);
    };
  }, [openDropdown]);

  // Set up the refs array
  useEffect(() => {
    // Initialize the refs array with the correct number of elements
    dropdownRefs.current = Array(3)
      .fill()
      .map((_, i) => dropdownRefs.current[i] || React.createRef());
  }, []);

  return (
    <header className={`${styles.header} ${scrolled ? styles.scrolled : ""}`}>
      <div className={styles.nav}>
        {/* Logo */}
        <div className={styles.logo}>
          {/* <Link href="/">
            <img src="/images/ppkb12.svg" alt="PepperKB Logo" />
          </Link> */}
        </div>

        {/* Hamburger Menu Button */}
        <div
          className={`${styles.menuIcon} ${isMenuOpen ? styles.open : ""}`}
          onClick={toggleMenu}
        >
          <span className={styles.bar}></span>
          <span className={styles.bar}></span>
          <span className={styles.bar}></span>
        </div>

        {/* Navigation Links */}
        <ul className={`${styles.navLinks} ${isMenuOpen ? styles.open : ""}`}>
          <li>
            <Link
              href="/"
              className={router.pathname === "/" ? styles.active : ""}
            >
              Home
            </Link>
          </li>

          {/* Browse Dropdown */}
          <li
            className={styles.dropdown}
            ref={(el) => (dropdownRefs.current[0] = el)}
            onMouseEnter={() => !isMobile && handleDropdownInteraction(0)}
            onMouseLeave={() => !isMobile && handleDropdownInteraction(null)}
          >
            <a
              href="#"
              onClick={(e) => handleDropdownInteraction(0, e)}
              className={openDropdown === 0 ? styles.active : ""}
            >
              Browse
              <span
                className={`${styles.arrow} ${
                  openDropdown === 0 ? styles.up : styles.down
                }`}
              ></span>
            </a>
            <div
              className={`${styles.dropdownContent} ${
                openDropdown === 0 ? styles.open : ""
              }`}
            >
              <Link href="/gene_families">TF Families</Link>
              <Link href="/snp_markers">SNP Markers</Link>
              <Link href="/gallery">Gallery</Link>
            </div>
          </li>

          {/* Tools Dropdown */}
          <li
            className={styles.dropdown}
            ref={(el) => (dropdownRefs.current[1] = el)}
            onMouseEnter={() => !isMobile && handleDropdownInteraction(1)}
            onMouseLeave={() => !isMobile && handleDropdownInteraction(null)}
          >
            <a
              href="#"
              onClick={(e) => handleDropdownInteraction(1, e)}
              className={openDropdown === 1 ? styles.active : ""}
            >
              Tools
              <span
                className={`${styles.arrow} ${
                  openDropdown === 1 ? styles.up : styles.down
                }`}
              ></span>
            </a>
            <div
              className={`${styles.dropdownContent} ${
                openDropdown === 1 ? styles.open : ""
              }`}
            >
              <Link href="/geneExpViz">Gene Expression Visualization</Link>
              <Link href="/blast">BLAST</Link>
              <Link href="/jbrowse2">JBrowse2</Link>
              <Link href="/genetool">GeneViz</Link>
            </div>
          </li>

          {/* Help Dropdown */}
          <li
            className={styles.dropdown}
            ref={(el) => (dropdownRefs.current[2] = el)}
            onMouseEnter={() => !isMobile && handleDropdownInteraction(2)}
            onMouseLeave={() => !isMobile && handleDropdownInteraction(null)}
          >
            <a
              href="#"
              onClick={(e) => handleDropdownInteraction(2, e)}
              className={openDropdown === 2 ? styles.active : ""}
            >
              Help
              <span
                className={`${styles.arrow} ${
                  openDropdown === 2 ? styles.up : styles.down
                }`}
              ></span>
            </a>
            <div
              className={`${styles.dropdownContent} ${
                openDropdown === 2 ? styles.open : ""
              }`}
            >
              <Link href="/manual">Manual</Link>
              <Link href="/references">References</Link>
            </div>
          </li>

          <li>
            <Link
              href="/contact"
              className={router.pathname === "/contact" ? styles.active : ""}
            >
              Contact us
            </Link>
          </li>

          {/* Mobile Search Bar (inside menu) - Only visible on mobile */}
          {isMobile && (
            <li className={styles.mobileSearchContainer}>
              <form className={styles.mobileSearchBar} onSubmit={handleSearch}>
                <Search size={18} className={styles.searchIcon} />
                <input
                  type="text"
                  placeholder="Search by Gene Id (Pn1.2085)"
                  className={styles.searchInput}
                  value={geneId}
                  onChange={(e) => setGeneId(e.target.value)}
                />
                {geneId && (
                  <button
                    type="button"
                    className={styles.clearButton}
                    onClick={clearSearch}
                    aria-label="Clear search"
                  >
                    <X size={16} />
                  </button>
                )}
                <button
                  type="submit"
                  className={styles.searchButton}
                  aria-label="Search"
                >
                  <Search size={18} />
                </button>
              </form>
            </li>
          )}
        </ul>

        {/* Desktop Search Bar (outside menu) - Only visible on desktop */}
        {!isMobile && (
          <form
            className={`${styles.searchBar} ${
              isSearchFocused ? styles.focused : ""
            }`}
            onSubmit={handleSearch}
          >
            <Search size={18} className={styles.searchIcon} />
            <input
              type="text"
              placeholder="Search by Gene ID (Pn1.2085)"
              className={styles.searchInput}
              value={geneId}
              onChange={(e) => setGeneId(e.target.value)}
              onFocus={() => setIsSearchFocused(true)}
              onBlur={() => setIsSearchFocused(false)}
              ref={searchInputRef}
            />
            {geneId && (
              <button
                type="button"
                className={styles.clearButton}
                onClick={clearSearch}
                aria-label="Clear search"
              >
                <X size={16} />
              </button>
            )}
            <button
              type="submit"
              className={styles.searchButton}
              aria-label="Search"
            >
              <Search size={18} />
            </button>
          </form>
        )}
      </div>
    </header>
  );
};

export default Header;
