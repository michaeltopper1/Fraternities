window.onload = function() {
  const el = document.getElementById('tpn-casecount-graph');

  if (el != null) {
    var chartColors = {
      studentsBlue: '#2980b9',
      wist: '#8e44ad',
      tq: '#1abc9c'
    };

    var config = {
      type: 'bar',
      data: {
        labels: ['Pitt students', 'Pitt employees', '15213 residents'],
        datasets: [
          {
            label: 'Pitt students',
            type: 'line',
            yAxisID: 'cases',
            backgroundColor: 'transparent',
            borderColor: chartColors.studentsBlue,
            pointBackgroundColor: chartColors.studentsBlue,
            tension: 0,
            fill: false
          },
          {
            label: 'Pitt employees',
            type: 'line',
            yAxisID: 'cases',
            backgroundColor: 'transparent',
            borderColor: chartColors.wist,
            pointBackgroundColor: chartColors.wist,
            tension: 0,
            fill: false
          },
          {
            label: '15213 residents',
            type: 'line',
            yAxisID: 'cases',
            backgroundColor: 'transparent',
            borderColor: chartColors.tq,
            pointBackgroundColor: chartColors.tq,
            tension: 0,
            fill: false
          },
        ]
      },
      plugins: [ChartDataSource],
      options: {
        title: {
          display: true,
          text: 'COVID-19 cases in Pitt and Oakland'
        },
        legend: { display: true },
        scales: {
          xAxes: [
            {
              scaleLabel: {
                display: true,
                labelString: 'Date'
              }
            },
          ],
          yAxes: [{
            id: 'cases',
            gridLines: { drawOnChartArea: false },
            scaleLabel: {
              display: true,
              labelString: 'Cases'
            }
          }]
        },
        plugins: {
          datasource: {
            type: 'csv',
            url: 'https://pittnews.com/casecount-graph/',
            delimiter: ',',
            rowMapping: 'dataset',
            datasetLabels: false,
            indexLabels: true,
          }
        }
      }
    };

    var ctx = el.getContext('2d');
    window.myChart = new Chart(ctx, config);
  }
};
