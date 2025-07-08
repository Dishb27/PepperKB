import React, { useState } from "react";
import PropTypes from "prop-types";
import styles from "../styles/snp_visualization.module.css";

const SNPVisualization = ({ snps = [], start, end }) => {
  const width = 600;
  const height = 200;
  const padding = 50;
  const triangleHeight = 8;
  const triangleBase = 5;
  const tickCount = 5;

  const [tooltip, setTooltip] = useState(null);

  const scaleX = (position) => {
    if (position < start || position > end) {
      console.warn(`SNP position ${position} is out of range.`);
      return null;
    }
    return (
      padding + ((position - start) / (end - start)) * (width - 2 * padding)
    );
  };

  const getTicks = () => {
    const ticks = [];
    const step = (end - start) / (tickCount + 1);
    for (let i = 1; i <= tickCount; i++) {
      ticks.push(start + i * step);
    }
    return ticks;
  };

  const handleTriangleClick = (snp) => {
    const pos = scaleX(snp.position);
    if (tooltip?.position === pos) {
      setTooltip(null);
    } else {
      setTooltip({
        position: pos,
        y: height / 2 - triangleHeight - 25,
        positionText: `Position: ${snp.position}`,
        alleleText: `${snp.ref} → ${snp.alt}`,
      });
    }
  };

  const ticks = getTicks();

  return (
    <svg width={width} height={height} className={styles.visualization}>
      <text
        x={width / 2}
        y={20}
        textAnchor="middle"
        fontSize="16"
        fontWeight="bold"
        fill="black"
      >
        SNP Location
      </text>

      <line
        x1={padding}
        y1={height / 2}
        x2={width - padding}
        y2={height / 2}
        stroke="black"
        strokeWidth="2"
      />

      {ticks.map((tick, index) => {
        const x = scaleX(tick);
        return (
          <g key={index}>
            <line
              x1={x}
              y1={height / 2 - 5}
              x2={x}
              y2={height / 2 + 5}
              stroke="black"
              strokeWidth="1"
            />
            <text
              x={x}
              y={height / 2 + 20}
              textAnchor="middle"
              fontSize="10"
              fill="black"
            >
              {Math.round(tick)}
            </text>
          </g>
        );
      })}

      <text x={padding} y={height / 2 + 40} textAnchor="middle" fontSize="12">
        {start} bp
      </text>
      <text
        x={width - padding}
        y={height / 2 + 40}
        textAnchor="middle"
        fontSize="12"
      >
        {end} bp
      </text>

      {(snps || []).map((snp, index) => {
        const x = scaleX(snp.position);
        if (x === null) return null;

        const points = `${x},${height / 2 - triangleHeight} ${
          x - triangleBase
        },${height / 2 + triangleHeight} ${x + triangleBase},${
          height / 2 + triangleHeight
        }`;

        return (
          <polygon
            key={index}
            points={points}
            fill="#ee2020"
            onClick={() => handleTriangleClick(snp)}
            style={{ cursor: "pointer" }}
          />
        );
      })}

      {tooltip && (
        <g>
          <rect
            x={tooltip.position - 70}
            y={tooltip.y - 35}
            width="140"
            height="40"
            className={styles.tooltip}
          />
          <text
            x={tooltip.position}
            y={tooltip.y - 20}
            className={styles["tooltip-text"]}
          >
            {tooltip.positionText}
          </text>
          <text
            x={tooltip.position}
            y={tooltip.y - 5}
            className={styles["tooltip-text"]}
          >
            {tooltip.alleleText}
          </text>
        </g>
      )}
    </svg>
  );
};

SNPVisualization.propTypes = {
  snps: PropTypes.arrayOf(
    PropTypes.shape({
      position: PropTypes.number.isRequired,
      ref: PropTypes.string.isRequired,
      alt: PropTypes.string.isRequired,
    })
  ).isRequired,
  start: PropTypes.number.isRequired,
  end: PropTypes.number.isRequired,
};

export default SNPVisualization;
