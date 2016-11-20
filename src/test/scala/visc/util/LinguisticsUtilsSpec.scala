package visc.util

import visc.test.UnitSpec

class LinguisticsUtilsSpec extends UnitSpec {

  "Accents and tones" should "be expanded correctly" in {
    LinguisticsUtils.expand('a') should contain theSameElementsAs List('a', 'á', 'à', 'ả', 'ã', 'ạ')
    LinguisticsUtils.expand('e', isOriginalIncluded = false) should contain theSameElementsAs List('é', 'è', 'ẻ', 'ẽ', 'ẹ')
    LinguisticsUtils.expand('a', isAccentExpanded = true) should contain theSameElementsAs List(
      'a', 'á', 'à', 'ả', 'ã', 'ạ',
      'ă', 'ắ', 'ằ', 'ẳ', 'ẵ', 'ặ',
      'â', 'ấ', 'ầ', 'ẩ', 'ẫ', 'ậ'
    )

    LinguisticsUtils.expand('b') should contain theSameElementsAs List('b')
    LinguisticsUtils.expand('d', isAccentExpanded = true) should contain theSameElementsAs List('đ')
  }

  it should "be reduced correctly" in {
    LinguisticsUtils.reduce('ắ') should be ('ă')
    LinguisticsUtils.reduce('ẩ', isAccentReduced = true) should be ('a')
    LinguisticsUtils.reduce('đ') should be ('đ')
    LinguisticsUtils.reduce('đ', isAccentReduced = true) should be ('d')
    LinguisticsUtils.expand('h') should contain theSameElementsAs List('h')
  }
}
