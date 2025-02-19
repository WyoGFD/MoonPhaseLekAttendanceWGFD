WITH OBS AS (
    SELECT obs.LocationID AS [ObsDBID],
        obs.LekObsYear AS [Year],
        obs.LekObsDate AS [Date],
        obs.LekObsTime AS [Time],
        obs.Males
    FROM dbo.LekObsFull as obs
    WHERE obs.LekObsDate IS NOT NULL
        AND obs.LekObsTime IS NOT NULL
        AND obs.LekObsYear >= 2000
),
SITE AS (
    SELECT sites.LekSiteID AS [LekDBID],
        CONCAT(sites.ManagementArea, '-', sites.LekName) AS LekID,
        sites.ManagementArea AS [LekMGMT],
        sites.Lat,
        sites.Lon,
        sites.[Zone],
        sites.UTMX,
        sites.UTMY
    FROM dbo.LekSitesFull AS sites
    WHERE sites.LekName IS NOT NULL
),
COMB AS (
    SELECT *
    FROM OBS
        INNER JOIN SITE ON OBS.[ObsDBID] = SITE.[LekDBID]
)
SELECT COMB.LekID,
    COMB.[LekMGMT],
    COMB.Lat,
    COMB.Lon,
    COMB.Zone,
    COMB.UTMX,
    COMB.UTMY,
    COMB.[Year],
    COMB.[Date],
    COMB.[Time],
    COMB.Males
FROM COMB
ORDER BY [LekID],
    [Year] ASC,
    [Date] ASC