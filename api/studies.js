// pages/api/studies.js
import pool from "../../../lib/db";

export default async function handler(req, res) {
  const { studyId, studyName } = req.query;

  try {
    const connection = await pool.getConnection();
    let query = "SELECT * FROM studies";
    let params = [];

    if (studyId) {
      query = "SELECT * FROM studies WHERE study_id = ?";
      params = [studyId];
    } else if (studyName) {
      query = "SELECT * FROM studies WHERE study_name = ?";
      params = [studyName];
    }

    const [rows] = await connection.execute(query, params);
    connection.release();

    res.status(200).json(rows);
  } catch (error) {
    console.error("Database error:", error);
    res.status(500).json({ error: "Failed to fetch studies data" });
  }
}
