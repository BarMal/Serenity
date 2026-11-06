const { chromium } = require('playwright');
const path = require('path');
(async () => {
  const browser = await chromium.launch();
  const out = process.argv[2];
  const file = 'file://' + path.join(__dirname, 'palette.html');
  for (const theme of ['dark', 'light'])
    for (const scale of [1, 2])
      for (const scene of ['line', 'export', 'empty', 'compact']) {
        const page = await browser.newPage({ viewport: { width: 1100, height: scene === 'compact' ? 300 : 470 }, deviceScaleFactor: scale });
        await page.goto(`${file}?theme=${theme}&scene=${scene}`);
        await page.screenshot({ path: path.join(out, `after-gui-${theme}-${scale}x-${scene}.png`) });
        await page.close();
      }
  await browser.close();
})();
