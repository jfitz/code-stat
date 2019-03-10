CREATE TABLE [Application].Cities(
    CityID int NOT NULL,
    CityName nvarchar(50) NOT NULL,
    StateProvinceID int NOT NULL,
    [Location] geography NULL,
    LatestRecordedPopulation bigint NULL,
    LastEditedBy int NOT NULL,
    ValidFrom datetime2(7) GENERATED ALWAYS AS ROW START NOT NULL,
    ValidTo datetime2(7) GENERATED ALWAYS AS ROW END NOT NULL,
 CONSTRAINT PK_Application_Cities PRIMARY KEY CLUSTERED 
(
    CityID ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON) ON USERDATA,
    PERIOD FOR SYSTEM_TIME (ValidFrom, ValidTo)
) ON USERDATA TEXTIMAGE_ON USERDATA
WITH
(
SYSTEM_VERSIONING = ON ( HISTORY_TABLE = [Application].Cities_Archive )
)