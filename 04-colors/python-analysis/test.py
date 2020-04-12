from webApp import AnalysisOutput

lighthouseText = open('../data/text/lighthouse-small.txt').read()

analysisOutput = AnalysisOutput('lighthouse', lighthouseText)

print(analysisOutput.makeHtml('lighthouse', lighthouseText))
