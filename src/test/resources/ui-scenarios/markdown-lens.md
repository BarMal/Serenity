# Scenario heading

Paragraph after heading.

This deliberately long paragraph wraps across a narrow deterministic viewport so the scenario can assert the actual source-lens rectangle rather than merely finding a source mapping after caret movement.

Second wrapped paragraph line keeps the paragraph block distinct from the heading while scrolling through a document larger than the viewport.

- first list item
- second list item

| name | value |
| --- | --- |
| one | 1 |

```scala
val deterministic = true
```

## Scrolled heading

The final paragraph also wraps across several visual rows when the narrow scenario scrolls to this region of the fixture.

- trailing list item one
- trailing list item two

| later | value |
| --- | --- |
| two | 2 |

```scala
val afterScroll = true
```
