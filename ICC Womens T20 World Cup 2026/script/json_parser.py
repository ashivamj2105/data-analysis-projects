import json
import glob
import pandas as pd
from pathlib import Path

#raw_data_directory = Path("C:/Users/aryam/Downloads/data-analysis-projects/ICC Womens T20 World Cup 2026/data/raw")
#processed_data_directory = Path("C:/Users/aryam/Downloads/data-analysis-projects/ICC Womens T20 World Cup 2026/data/processed")

raw_data_directory = Path("ICC Womens T20 World Cup 2026/data/raw")
processed_data_directory = Path("ICC Womens T20 World Cup 2026/data/processed")


all_deliveries = []

for file_path in raw_data_directory.glob("*.json"):
    with open(file_path, 'r') as f:
        data = json.load(f)
        
    info = data.get("info", {})
    match_id = file_path.stem
    date = info.get("dates", [None])[0]
    venue = info.get("venue")
    stage = info.get("event", {}).get("stage", "Group Stage")
    
    teams = info.get("teams", [])
    if len(teams) >= 2:
        team_x, team_y = teams[0], teams[1]
    else: 
        continue
    
    for innings_index, inning in enumerate(data.get("innings", [])):
        team_batting = inning.get("team")
        team_bowling = team_y if team_batting == team_x else team_x
        
        innings_no = innings_index + 1
        
        for over_data in inning.get("overs", []):
            over_number = over_data.get("over")
            
            for ball_index, delivery in enumerate(over_data.get("deliveries", [])):
                batter_runs = delivery.get("runs", {}).get("batter", 0)
                
                wides = delivery.get("extras", {}).get("wides", 0)
                noballs = delivery.get("extras", {}).get("noballs", 0)
                byes = delivery.get("extras", {}).get("byes", 0)
                legbyes = delivery.get("extras", {}).get("legbyes", 0)
                
                ball_data = {
                    "match_id" : match_id,
                    "date": date,
                    "venue": venue,
                    "stage": stage,
                    "innings_no": innings_no,
                    "batting_team": team_batting,
                    "bowling_team": team_bowling,
                    "over": over_number,
                    "ball" : ball_index + 1,
                    "batter": delivery.get("batter"),
                    "bowler": delivery.get("bowler"),
                    "non_striker": delivery.get("non_striker"),
                    "batter_runs": batter_runs,
                    "extra_runs": delivery.get("runs", {}).get("extras", 0),
                    "total_runs" : delivery.get("runs", {}).get("total", 0),
                    
                    "wides": wides,
                    "noballs": noballs,
                    "byes": byes,
                    "legbyes": legbyes,
                    
                    "team_ball_faced" : 0 if wides > 0 else 1,
                    
                    "is_four": 1 if batter_runs == 4 else 0,
                    "is_six": 1 if batter_runs == 6 else 0,
                    "is_boundary": 1 if batter_runs in [4, 6] else 0
                }
                
                if "wickets" in delivery:
                    wicket= delivery["wickets"][0]
                    ball_data["is_wicket"] = 1
                    ball_data["player_out"] = wicket.get("player_out")
                    ball_data["wicket_kind"] = wicket.get("kind")
                    ball_data["fielder"] = wicket.get("fielders", [{}])[0].get("name") if wicket.get("fielders") else None
                else: 
                    ball_data["is_wicket"] = 0
                    ball_data["player_out"] = None
                    ball_data["wicket_kind"] = None
                    ball_data["fielder"] = None
                    
                all_deliveries.append(ball_data)

df = pd.DataFrame(all_deliveries)

processed_data_directory.mkdir(parents = True, exist_ok = True)
df.to_parquet(processed_data_directory/ 'master_deliveries.parquet', index = False) # Parquet - data organized by columns
print("Successful")