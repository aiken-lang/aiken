import blueprint from '../../plutus.json' assert { type: 'json' };

export type Validators = {
  giftCard: string;
};

export function readValidators(): Validators {
  const giftCard = blueprint.validators.find((v) => v.title === 'oneshot.gift_card.spend');

  if (!giftCard) {
    throw new Error('Gift Card validator not found');
  }

  return {
    giftCard: giftCard.compiledCode
  };
}
