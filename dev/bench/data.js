window.BENCHMARK_DATA = {
  "lastUpdate": 1781724600196,
  "repoUrl": "https://github.com/AndrewLiu0/jsip-exchange",
  "entries": {
    "Order book benchmark": [
      {
        "commit": {
          "author": {
            "email": "72947325+AndrewLiu0@users.noreply.github.com",
            "name": "Andrew Liu",
            "username": "AndrewLiu0"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "1d383f4aeb3ceaf31a64d665171fc5494147414b",
          "message": "Merge branch 'jane-street-immersion-program:main' into main",
          "timestamp": "2026-06-17T15:25:06-04:00",
          "tree_id": "b105f708f1d0a3bfac0fc8f703926fc5cb5958f3",
          "url": "https://github.com/AndrewLiu0/jsip-exchange/commit/1d383f4aeb3ceaf31a64d665171fc5494147414b"
        },
        "date": 1781724599907,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "find_match (n=10)",
            "value": 21.56356531748929,
            "unit": "ns"
          },
          {
            "name": "find_match (n=50)",
            "value": 22.46916715716691,
            "unit": "ns"
          },
          {
            "name": "find_match (n=100)",
            "value": 21.75361285283642,
            "unit": "ns"
          },
          {
            "name": "find_match (n=500)",
            "value": 21.45987024444405,
            "unit": "ns"
          },
          {
            "name": "find_match_miss (n=10)",
            "value": 109.79868009724917,
            "unit": "ns"
          },
          {
            "name": "find_match_miss (n=50)",
            "value": 501.6513426149992,
            "unit": "ns"
          },
          {
            "name": "find_match_miss (n=100)",
            "value": 1086.2298681921695,
            "unit": "ns"
          },
          {
            "name": "find_match_miss (n=500)",
            "value": 5356.099552794058,
            "unit": "ns"
          },
          {
            "name": "best_bid_offer (n=10)",
            "value": 199.64001852645313,
            "unit": "ns"
          },
          {
            "name": "best_bid_offer (n=50)",
            "value": 917.1945781902283,
            "unit": "ns"
          },
          {
            "name": "best_bid_offer (n=100)",
            "value": 1901.737254663676,
            "unit": "ns"
          },
          {
            "name": "best_bid_offer (n=500)",
            "value": 9371.143525807012,
            "unit": "ns"
          },
          {
            "name": "add+remove (n=100)",
            "value": 1577.7545427819987,
            "unit": "ns"
          },
          {
            "name": "submit_ioc_cross (n=10)",
            "value": 1155.2927247226048,
            "unit": "ns"
          },
          {
            "name": "submit_ioc_cross (n=50)",
            "value": 4993.4777014727315,
            "unit": "ns"
          },
          {
            "name": "submit_ioc_cross (n=100)",
            "value": 9372.004363074815,
            "unit": "ns"
          },
          {
            "name": "submit_ioc_cross (n=500)",
            "value": 45196.75624606295,
            "unit": "ns"
          },
          {
            "name": "submit_ioc_miss (n=10)",
            "value": 583.5159547376231,
            "unit": "ns"
          },
          {
            "name": "submit_ioc_miss (n=50)",
            "value": 2568.931857188814,
            "unit": "ns"
          },
          {
            "name": "submit_ioc_miss (n=100)",
            "value": 5027.471617601448,
            "unit": "ns"
          },
          {
            "name": "submit_ioc_miss (n=500)",
            "value": 24582.9565205841,
            "unit": "ns"
          },
          {
            "name": "submit_sweep_10_levels",
            "value": 4910.244097521401,
            "unit": "ns"
          },
          {
            "name": "submit_sweep_50_levels",
            "value": 78341.43683254386,
            "unit": "ns"
          },
          {
            "name": "submit_sweep_100_levels",
            "value": 292342.15437876224,
            "unit": "ns"
          },
          {
            "name": "find_match_alloc (n=100)",
            "value": 22.470255773886194,
            "unit": "ns"
          }
        ]
      }
    ]
  }
}