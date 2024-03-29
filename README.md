# STA3034
## 차트 조작 탐지를 위한 음원 차트 분석 결과 대시보드
본 repository는 대용량자료분석과시각화 수업의 과제에서 진행했던 프로젝트를 담고 있으며, Rshiny을 활용해 만든 interactive 시각화 대시보드(웹 앱)를 담고 있습니다.  

## 실행방법
```R
# in RStudio
shiny::runGitHub('STA3034', 'victolee0')
```
## 진행 과정

### 데이터 수집
파이썬 (BeatifulSoup, Selenium) -> 멜론 주간 차트 크롤링  

### EDA
* 2010년대 이전과 이후 데이터에서 좋아요 수에 유의미한 차이 확인  
 => 2012년 이후의 데이터만 분석에 사용  
* 콜라보레이션 음원이 차트에서 보이는 특징이 있지 않을까?  
 => 단발성으로 그치는 경향 확인
* 발매 직후에 차트에 진입하는 겻이 일반적임 확인  
 발매 ~ 차트 진입 기간이 3주 이상인 곡에 팝송이 많음 확인  
 => 한국 음원의 데이터만 분석에 사용
 
* 차트에 올라간 음원은 순위 하락이 일반적임을 확인  
 발매 ~ 차트 진입 기간이 3주 이상인 곡은 일정 기간 순위 상승 이후 하락 추세에 진입 확인
 => 음원의 순위 변동을 이용해 음원의 특성을 구분할 수 있겠다고 생각함
 
 ### 모델링  
 
 * Dynamic Time Warping Clustering 사용  
    - 음원마다 차트에 있는 기간이 다른데, 이와 상관없이 이용 가능한 방법  
    - 차트 조작이 확인된 음원이 없기 때문에 비지도학습 알고리즘을 사용하여 모델링 진행  
    - 차트에 머무른 기간이 짧은 데이터는 순위 변동을 확인하기 어려워 4주 이상 머무른 데이터만 분석에 이용
  
 * 클러스터의 개수를 정하기 위해 **Silhouette index**, **Dunn index** 등 여러 지표를 고려
 
 
### 결과
- 총 5개의 클러스터로 음원을 분류하였으며, 그 중 가장 적은 음원이 포함된 클러스터에는 전체 음원의 3.4%에 해당하는 음원이 속했다.  
- 차트 조작으로 의심받는 음원들이 어느 클러스터에 있는지 확인한 결과, 가장 적은 음원이 포함된 클러스터에 20%의 차트 조작 의심 음원이 포함된 결과(전체 클러스터에서 2위)를 확인
- 클러스터링 결과를 Shiny를 이용하여 확인할 수 있게 대시보드 형태로 구현
