// pages/api/snp.js
import pool from "../../../lib/db";

export default async function handler(req, res) {
  const { studyId, page = 1, pageSize = 5000 } = req.query;

  if (!studyId) {
    return res.status(400).json({ error: "Study ID is required" });
  }

  try {
    const connection = await pool.getConnection();

    // Convert to numbers to ensure type safety
    const pageNum = parseInt(page, 10);
    const pageSizeNum = parseInt(pageSize, 10);
    const offset = (pageNum - 1) * pageSizeNum;

    // Count total SNPs for pagination
    const [countResult] = await connection.execute(
      "SELECT COUNT(*) as total FROM snp WHERE study_id = ?",
      [studyId]
    );
    const totalSNPs = countResult[0].total;
    const totalPages = Math.ceil(totalSNPs / pageSizeNum);

    // Modify the query to use direct values instead of prepared statement parameters for LIMIT and OFFSET
    const [rows] = await connection.execute(
      `SELECT * FROM snp WHERE study_id = ? LIMIT ${pageSizeNum} OFFSET ${offset}`,
      [studyId]
    );

    connection.release();

    res.status(200).json({
      data: rows,
      pagination: {
        totalSNPs,
        totalPages,
        currentPage: pageNum,
        pageSize: pageSizeNum,
      },
    });
  } catch (error) {
    console.error("Database error:", error);
    res.status(500).json({
      error: "Failed to fetch SNP data",
      details: error.message,
    });
  }
}

// Optional: Increase response size limit
export const config = {
  api: {
    responseLimit: "8mb",
  },
};
