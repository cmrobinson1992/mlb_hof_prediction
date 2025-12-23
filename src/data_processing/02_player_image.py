"""
Fetch MLB player headshots for both active and historical players.

This script supports two modes:
1. Active rosters: Fetches current MLB team rosters via Stats API
2. Historical/all players: Uses a list of bbref IDs (e.g., from Lahman database)
   and looks up their MLB AM IDs via pybaseball

Usage:
    # Active players only
    python 02_fetch_player_images.py

    # All players from a CSV with bbrefID column
    python 02_fetch_player_images.py --player-list data/processed/all_players.csv
"""

import os
import time
import argparse
import requests
import pandas as pd

from pybaseball import playerid_reverse_lookup, playerid_lookup

MLB_STATSAPI = "https://statsapi.mlb.com/api/v1"
HEADSHOT_URL = (
    "https://img.mlbstatic.com/mlb-photos/image/upload/"
    "w_213,d_people:generic:headshot:silo:current.png,q_auto:best,f_auto/"
    "v1/people/{mlbam_id}/headshot/67/current"
)


def get_json(url, params=None, timeout=30):
    """Fetch JSON from a URL with error handling."""
    r = requests.get(url, params=params, timeout=timeout)
    r.raise_for_status()
    return r.json()


def get_active_mlb_teams():
    """Get list of active MLB teams."""
    data = get_json(f"{MLB_STATSAPI}/teams", params={"sportId": 1})
    return [{"team_id": t["id"], "team_name": t["name"]} for t in data.get("teams", [])]


def get_active_roster(team_id):
    """Get active roster for a specific team."""
    data = get_json(
        f"{MLB_STATSAPI}/teams/{team_id}/roster",
        params={"rosterType": "active", "hydrate": "person"}
    )
    rows = []
    for p in data.get("roster", []):
        person = p.get("person", {})
        mlbam_id = person.get("id")
        name = person.get("fullName")
        pos = (p.get("position") or {}).get("abbreviation")
        rows.append({
            "mlbamID": mlbam_id,
            "name": name,
            "pos": pos,
            "team_id": team_id,
            "is_active": True
        })
    return rows


def get_40_man_roster(team_id):
    """Get 40-man roster for a specific team (includes injured/minors)."""
    data = get_json(
        f"{MLB_STATSAPI}/teams/{team_id}/roster",
        params={"rosterType": "40Man", "hydrate": "person"}
    )
    rows = []
    for p in data.get("roster", []):
        person = p.get("person", {})
        mlbam_id = person.get("id")
        name = person.get("fullName")
        pos = (p.get("position") or {}).get("abbreviation")
        status = p.get("status", {}).get("description", "Active")
        rows.append({
            "mlbamID": mlbam_id,
            "name": name,
            "pos": pos,
            "team_id": team_id,
            "status": status,
            "is_active": True
        })
    return rows


def get_free_agents(season=None):
    """
    Attempt to get free agents from MLB Stats API.
    
    Note: The MLB Stats API free agent endpoints are unreliable/deprecated.
    For comprehensive free agent coverage, use --player-list with your 
    Lahman data to catch players who played recently but aren't on rosters.
    """
    from datetime import datetime
    current_year = datetime.now().year
    seasons_to_try = [season] if season else [current_year, current_year - 1]
    
    # Try various endpoint formats that MLB has used
    endpoint_formats = [
        "{base}/sports/1/players?season={season}&gameType=R",
        "{base}/sports/1/players?season={season}",
    ]
    
    for try_season in seasons_to_try:
        for endpoint_fmt in endpoint_formats:
            try:
                url = endpoint_fmt.format(base=MLB_STATSAPI, season=try_season)
                data = get_json(url)
                
                # Filter to players without a current team
                rows = []
                for p in data.get("people", []):
                    if not p.get("currentTeam"):
                        rows.append({
                            "mlbamID": p.get("id"),
                            "name": p.get("fullName"),
                            "pos": p.get("primaryPosition", {}).get("abbreviation"),
                            "team_id": None,
                            "team": "Free Agent",
                            "is_active": True
                        })
                
                if rows:
                    print(f"Found {len(rows)} free agents/unsigned players from {try_season}")
                    return rows
                    
            except Exception:
                continue
    
    # If API methods fail, suggest alternative
    print("Note: MLB API free agent data unavailable. For comprehensive coverage,")
    print("      use --player-list with a CSV of recent players from Lahman data.")
    return []


def lookup_mlbam_from_bbref(bbref_ids, batch_size=100):
    """
    Look up MLB AM IDs from Baseball Reference IDs using pybaseball.
    Processes in batches to avoid API limits.
    """
    all_results = []

    for i in range(0, len(bbref_ids), batch_size):
        batch = bbref_ids[i:i + batch_size]
        print(f"  Looking up batch {i // batch_size + 1} ({len(batch)} players)...")

        try:
            lookup = playerid_reverse_lookup(batch, key_type="bbref")
            lookup = lookup.rename(columns={
                "key_bbref": "bbrefID",
                "key_mlbam": "mlbamID",
                "name_first": "first_name",
                "name_last": "last_name"
            })
            all_results.append(lookup)
        except Exception as e:
            print(f"  Warning: Batch lookup failed: {e}")

        time.sleep(0.5)  # Be polite to the API

    if all_results:
        return pd.concat(all_results, ignore_index=True)
    return pd.DataFrame()


def fetch_all_current_players(include_40_man=True):
    """
    Fetch all current MLB players including:
    - Active rosters (or 40-man rosters)
    - Free agents
    """
    teams = get_active_mlb_teams()
    team_map = {t["team_id"]: t["team_name"] for t in teams}

    print(f"Fetching rosters from {len(teams)} teams...")
    roster_rows = []

    for t in teams:
        if include_40_man:
            roster_rows.extend(get_40_man_roster(t["team_id"]))
        else:
            roster_rows.extend(get_active_roster(t["team_id"]))
        time.sleep(0.15)

    # Add free agents
    print("Fetching free agents...")
    fa_rows = get_free_agents()
    roster_rows.extend(fa_rows)

    df = pd.DataFrame(roster_rows)
    df = df.dropna(subset=["mlbamID"]).drop_duplicates(subset=["mlbamID"])
    df["mlbamID"] = df["mlbamID"].astype(int)
    df["team"] = df["team_id"].map(team_map).fillna(df.get("team", "Unknown"))

    # Look up bbref IDs
    print("Looking up Baseball Reference IDs...")
    lookup = playerid_reverse_lookup(df["mlbamID"].tolist(), key_type="mlbam")
    lookup = lookup.rename(columns={"key_mlbam": "mlbamID", "key_bbref": "bbrefID"})

    df = df.merge(lookup[["mlbamID", "bbrefID"]], on="mlbamID", how="left")
    df["headshot_url"] = df["mlbamID"].apply(lambda x: HEADSHOT_URL.format(mlbam_id=x))

    return df


def load_player_file(filepath):
    """
    Load player data from CSV or RDS file.
    
    Args:
        filepath: Path to CSV or RDS file
        
    Returns:
        pandas DataFrame
    """
    filepath_lower = str(filepath).lower()
    
    if filepath_lower.endswith('.rds') or filepath_lower.endswith('.rda') or filepath_lower.endswith('.rdata'):
        # Try multiple methods to read R data files
        
        # Method 1: pyreadr (preferred, pure Python)
        try:
            import pyreadr
            result = pyreadr.read_r(filepath)
            df = list(result.values())[0]
            print(f"Loaded RDS file using pyreadr")
            return df
        except ImportError:
            pass
        except Exception as e:
            print(f"pyreadr failed: {e}")
        
        # Method 2: rpy2 (requires R installation)
        try:
            import rpy2.robjects as ro
            from rpy2.robjects import pandas2ri
            from rpy2.robjects.packages import importr
            
            pandas2ri.activate()
            base = importr('base')
            
            result = ro.r(f'readRDS("{filepath}")')
            df = pandas2ri.rpy2py(result)
            print(f"Loaded RDS file using rpy2")
            return df
        except ImportError:
            pass
        except Exception as e:
            print(f"rpy2 failed: {e}")
        
        # If we get here, neither method worked
        raise ImportError(
            f"\nCannot read RDS file: {filepath}\n\n"
            "To read RDS files, you need one of these options:\n\n"
            "  Option 1 (recommended): Install pyreadr via conda\n"
            "      conda install -c conda-forge pyreadr\n\n"
            "  Option 2: Install rpy2 (requires R to be installed)\n"
            "      pip install rpy2\n\n"
            "  Option 3: Convert to CSV in R first:\n"
            "      df <- readRDS('your_file.rds')\n"
            "      write.csv(df, 'your_file.csv', row.names = FALSE)\n"
            "    Then use --player-list your_file.csv\n"
        )
    else:
        # Assume CSV for anything else
        return pd.read_csv(filepath)


def fetch_players_from_list(player_file, id_column="bbrefID"):
    """
    Fetch player info and headshots for a list of players from a file.
    Useful for historical players not on current rosters.

    Args:
        player_file: Path to CSV or RDS file with player IDs
        id_column: Column name containing the player IDs (bbrefID or playerID)
    """
    print(f"Loading player list from {player_file}...")
    players = load_player_file(player_file)

    # Handle Lahman's 'playerID' column name
    if id_column not in players.columns and "playerID" in players.columns:
        id_column = "playerID"

    bbref_ids = players[id_column].dropna().unique().tolist()
    print(f"Found {len(bbref_ids)} unique player IDs")

    # Look up MLB AM IDs
    print("Looking up MLB AM IDs (this may take a while)...")
    lookup_df = lookup_mlbam_from_bbref(bbref_ids)

    if lookup_df.empty:
        print("Warning: Could not look up any player IDs")
        return pd.DataFrame()

    # Build result dataframe
    lookup_df["name"] = lookup_df["first_name"] + " " + lookup_df["last_name"]
    lookup_df["mlbamID"] = lookup_df["mlbamID"].astype("Int64")  # nullable int

    # Only keep rows with valid MLB AM IDs (needed for headshots)
    valid_df = lookup_df.dropna(subset=["mlbamID"])
    print(f"Found MLB AM IDs for {len(valid_df)} of {len(bbref_ids)} players")

    valid_df["headshot_url"] = valid_df["mlbamID"].apply(
        lambda x: HEADSHOT_URL.format(mlbam_id=int(x)) if pd.notna(x) else None
    )
    valid_df["is_active"] = False
    valid_df["team"] = "Historical"

    return valid_df


def download_headshots(df, img_dir="data/external/headshots", skip_existing=True):
    """Download headshot images for all players in dataframe."""
    os.makedirs(img_dir, exist_ok=True)

    downloaded = 0
    skipped = 0
    failed = 0

    for _, row in df.iterrows():
        if pd.isna(row.get("headshot_url")):
            continue

        # Use bbrefID as filename if available, otherwise mlbamID
        if pd.notna(row.get("bbrefID")):
            fname = row["bbrefID"]
        else:
            fname = str(int(row["mlbamID"]))

        path = os.path.join(img_dir, f"{fname}.jpg")

        if skip_existing and os.path.exists(path):
            skipped += 1
            continue

        try:
            img = requests.get(row["headshot_url"], timeout=30)
            img.raise_for_status()
            with open(path, "wb") as f:
                f.write(img.content)
            downloaded += 1
        except Exception as e:
            print(f"Failed {row.get('name', 'Unknown')} ({row.get('mlbamID')}): {e}")
            failed += 1

        time.sleep(0.10)

    print(f"Downloaded: {downloaded}, Skipped: {skipped}, Failed: {failed}")
    return img_dir


def main():
    parser = argparse.ArgumentParser(
        description="Fetch MLB player headshots for active and historical players"
    )
    parser.add_argument(
        "--player-list",
        type=str,
        default=None,
        help="CSV or RDS file with player IDs (bbrefID or playerID column). "
             "If not provided, fetches current MLB rosters."
    )
    parser.add_argument(
        "--supplement-with-list",
        type=str,
        default=None,
        help="CSV or RDS file to supplement roster data with. Fetches rosters first, "
             "then adds any players from this list who aren't on rosters. "
             "Useful for catching free agents and recently retired players."
    )
    parser.add_argument(
        "--id-column",
        type=str,
        default="bbrefID",
        help="Column name containing player IDs (default: bbrefID)"
    )
    parser.add_argument(
        "--output-csv",
        type=str,
        default="data/processed/mlb_headshots_with_bbref.csv",
        help="Output CSV path"
    )
    parser.add_argument(
        "--download-images",
        action="store_true",
        help="Download headshot images"
    )
    parser.add_argument(
        "--image-dir",
        type=str,
        default="data/external/headshots",
        help="Directory to save headshot images"
    )
    parser.add_argument(
        "--include-40-man",
        action="store_true",
        help="Include full 40-man rosters instead of just active roster"
    )

    args = parser.parse_args()

    # Fetch player data
    if args.player_list:
        # Historical/custom player list mode
        df = fetch_players_from_list(args.player_list, args.id_column)
    else:
        # Current MLB players mode
        df = fetch_all_current_players(include_40_man=args.include_40_man)
        
        # Supplement with additional player list if provided
        if args.supplement_with_list:
            print(f"\nSupplementing roster data with {args.supplement_with_list}...")
            supplement_df = fetch_players_from_list(args.supplement_with_list, args.id_column)
            
            if not supplement_df.empty:
                # Find players in supplement list but not in roster
                roster_bbref_ids = set(df["bbrefID"].dropna())
                roster_mlbam_ids = set(df["mlbamID"].dropna())
                
                # Keep supplement players not already in roster
                mask = ~(
                    supplement_df["bbrefID"].isin(roster_bbref_ids) | 
                    supplement_df["mlbamID"].isin(roster_mlbam_ids)
                )
                new_players = supplement_df[mask].copy()
                new_players["team"] = "Not on 40-man"
                
                if len(new_players) > 0:
                    print(f"Adding {len(new_players)} players not on current rosters")
                    df = pd.concat([df, new_players], ignore_index=True)
                else:
                    print("All players in supplement list are already on rosters")

    if df.empty:
        print("No players found!")
        return

    # Select and order columns for output
    output_cols = ["bbrefID", "mlbamID", "name", "team", "pos", "headshot_url", "is_active"]
    output_cols = [c for c in output_cols if c in df.columns]
    df = df[output_cols].sort_values(["team", "name"] if "team" in df.columns else ["name"])

    # Save CSV
    os.makedirs(os.path.dirname(args.output_csv) or ".", exist_ok=True)
    df.to_csv(args.output_csv, index=False)
    print(f"Wrote {len(df):,} rows to {args.output_csv}")

    # Download images if requested
    if args.download_images:
        download_headshots(df, args.image_dir)


if __name__ == "__main__":
    main()